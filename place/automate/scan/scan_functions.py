#UPDATE PLOTTER
"""Static classes to assist in scanning.

This file contains static classes containing functions used to perform scans.

Note: the commmand-line options used to be here, but are now in a separate
file, named cli.py

@author: Jami L Johnson, Evan Rust
March 19 2015
"""
from __future__ import print_function

import re
from math import ceil
from time import sleep, time
import numpy as np
from obspy.core.trace import Stats
from obspy import read, Trace, UTCDateTime
import matplotlib.pyplot as plt
import scipy.signal as sig

# place modules
from ..new_focus.picomotor import PMot
from ..polytec.vibrometer import Polytec
from ..quanta_ray.QRay_driver import QuantaRay

def picomotor(motor_num):
    """Initialize PicoMotor"""
    motor = PMot(motor_num)
    print('PicoMotor initialized')
    return motor

def quanta_ray(percent, par):
    """Starts Laser in rep-rate mode and sets watchdog time.

    :returns: the repitition rate of the laser.
    """

    # open laser connection
    QuantaRay().openConnection()
    QuantaRay().setWatchdog(time=100)
    # turn laser on
    QuantaRay().on()
    sleep(20)

    # set-up laser
    QuantaRay().set(cmd='SING') # set QuantaRay to single shot
    QuantaRay().set(cmd='NORM')
    QuantaRay().setOscPower(percent) # set power of laser
    sleep(1)

    print('Power of laser oscillator: ' + QuantaRay().getOscPower())

    # get rep-rate
    rep_rate = QuantaRay().getTrigRate()
    rep_rate = re.findall(r'[-+]?\d*\.\d+|\d+', rep_rate) # get number only
    rep_rate_float = float(rep_rate[0])
    trace_time = par['AVERAGES']/rep_rate_float

    # set watchdog time > time of one trace, so laser doesn't turn off between commands
    QuantaRay().setWatchdog(time=ceil(2*trace_time))

    return trace_time

def header(par):
    """Initialize generic trace header for all traces"""

    custom_header = Stats()
    if par['IMPEDANCE'] == 1:
        impedance = '1Mohm'
    else:
        impedance = '50 ohms'
    if par['IMPEDANCE2'] == 1:
        impedance2 = '1Mohm'
    else:
        impedance2 = '50 ohms'
    custom_header.impedance = impedance
    custom_header.impedance2 = impedance2
    custom_header.x_position = par['I1']
    custom_header.max_frequency = par['MAX_FREQ']
    custom_header.receiver = par['RECEIVER']
    custom_header.decoder = par['DECODER']
    custom_header.decoder_range = par['DECODER_RANGE']
    custom_header.source_energy = par['ENERGY']
    custom_header.wavelength = par['WAVELENGTH']
    custom_header.x_unit = 'mm'
    custom_header.theta_unit = 'deg'
    custom_header.y_unit = 'mm'
    custom_header.comments = par['COMMENTS']
    custom_header.averages = par['AVERAGES']
    custom_header.calib_unit = par['CALIB_UNIT']
    custom_header.time_delay = par['TIME_DELAY']
    custom_header.scan_time = ''
    custom_header.focus = 0

    header = Stats(custom_header)
    if par['RECEIVER'] == 'polytec':
        if par['DECODER'] == 'DD-300' and par['IMPEDANCE'] == 1:
            header.calib = 25
        else:
            header.calib = par['CALIB']
    header.channel = par['CHANNEL']

    return header

def two_plot(group_name, header):
    plt.ion()
    plt.show()
    fig = plt.figure()
    axis_1 = fig.add_subplot(211)
    if header.calib_unit.rstrip() == 'nm/V':
        axis_1.set_ylabel('Displacement (nm)')
    elif header.calib_unit.rstrip() == 'mm/s/V':
        axis_1.set_ylabel('Particle Velocity (mm/s)')
    axis_1.set_xlabel(r'Time ($\mu$s)')
    axis_1.set_title('Last Trace Acquired')
    axis_2 = fig.add_subplot(212)
    if group_name in ['LONG_STAGE', 'SHORT_STAGE', 'PICOMOTOR-X', 'PICOMOTOR-Y']:
        axis_2.set_ylabel('Scan Location ('+ header.x_unit + ')')
    elif group_name == 'ROT_STAGE':
        axis_2.set_ylabel('Scan Location ('+ header.theta_unit + ')')
    axis_2.set_xlabel(r'Time ($\mu$s)')

    return axis_1, axis_2, fig

def get_times(control, channel, header):
    times = control.getTimesOfRecord()
    dt_value = times[1]-times[0]
    header.delta = dt_value
    return times, header

def butter_lowpass(cutoff, fs, order=5):
    nyq = 0.5 * fs
    normal_cutoff = cutoff / nyq
    return sig.butter(order, normal_cutoff, btype='low', analog=False)

def butter_lowpass_filter(data, fs_value):
    cutoff = np.array(5e6)
    order = 2
    b_value, a_value = butter_lowpass(cutoff, fs_value, order=order)
    return sig.filtfilt(b_value, a_value, data)

def butter_bandpass(wn_value, fs, order=5):
    nyq = 0.5 * fs
    normal_cutoff_low = wn_value[0] / nyq
    normal_cutoff_high = wn_value[1] / nyq
    return sig.butter(
        order,
        [normal_cutoff_low, normal_cutoff_high],
        btype='band',
        analog=False,
        )

def butter_bandpass_filter(data, freqs, carrier):
    wn_value = np.array([carrier - 5*10**6, carrier + 5*10**6])
    order = 2
    b_value, a_value = butter_bandpass(wn_value, freqs, order)
    return sig.filtfilt(b_value, a_value, data)

def osldv_process(records, records2, par):
    """Function for processing I & Q from OSLDV receiver"""
    print('begin processing...')
    fd_value = 2/(1550*1e-9)

    for index in range(0, len(records)):
        time1 = time()
        sample_rate = par['CONTROL'].samplesPerSec
        q_value = butter_lowpass_filter(records[index], sample_rate)
        i_value = butter_lowpass_filter(records2[index], sample_rate)

        # MEASURE FREQUENCY FROM Q & I
        vfm = np.array(
            (i_value[0:-1]*np.diff(q_value) - q_value[0:-1]*np.diff(i_value))
            / (i_value[0:-1]**2 + q_value[0:-1]**2)
            )
        end = vfm[-1]
        vfm[0] = 0.0

        # FREQUENCY TO mm/s
        vfm = sample_rate*np.concatenate((vfm, [end]))/(2*np.pi*fd_value)*1000

        records[index] = (vfm)

    return records


def data_capture(par):
    """Capture data for up to two channels, and OSLDV pre-averaging
    processing
    """
    par['CONTROL'].start_capture()
    par['CONTROL'].readData()

    if par['RECEIVER'] == 'osldv':
        par['CONTROL2'].start_capture()
        par['CONTROL2'].readData()

        # get I & Q
        records = par['CONTROL'].getDataRecordWise(par['CHANNEL'])
        records2 = par['CONTROL2'].getDataRecordWise(par['CHANNEL2'])
        par['CONTROL'].endCapture()

        # calculate partical velocity from I & Q
        records = osldv_process(records, records2, par)

        average = np.average(records, 0)
        average2 = []

    else:
        # two channel acquisition
        if par['CHANNEL2'] != 'null':
            par['CONTROL2'].start_capture()
            par['CONTROL2'].readData()

        # record channel 1
        records = par['CONTROL'].getDataRecordWise(par['CHANNEL'])
        average = np.average(records, 0)

        if par['CHANNEL2'] != 'null':
            # record channel 2
            records2 = par['CONTROL2'].getDataRecordWise(par['CHANNEL2'])
            average2 = np.average(records2, 0)
        else:
            average2 = []

    return average, average2

def update_header(header, x_value, group_name=''):
    header.starttime = UTCDateTime()
    if group_name in ['LONG_STAGE', 'PICOMOTOR-X']:
        header.x_position = x_value
    elif group_name == 'ROT_STAGE':
        header.theta_position = x_value
    elif group_name in ['SHORT_STAGE', 'PICOMOTOR-Y']:
        header.y_position = x_value
    else:
        header.x_position = x_value

def move_stage(group_name, xps, socket_id, x_value):
    if group_name in ['LONG_STAGE', 'SHORT_STAGE', 'ROT_STAGE']:
        xps.GroupMoveAbsolute(socket_id, group_name, [x_value])
        actual_pos = xps.GroupPositionCurrentGet(socket_id, group_name, 1)
        return actual_pos[1]

def save_trace(header, average, filename):
    header.npts = len(average)
    trace = Trace(data=average, header=header)
    trace.write(filename, 'H5', mode='a')
    return

def update_time(par):
    """calculate time remaining"""
    par['TOTAL_TIME'] -= par['TRACE_TIME']
    hour_left = int(par['TOTAL_TIME']/3600)
    less_hour = par['TOTAL_TIME']- hour_left*3600
    min_left = int(less_hour/60)
    sec_left = int(less_hour - min_left*60)
    print(str(hour_left) + ':' + str(min_left) + ':' + str(sec_left) + ' remaining')
    return par

def check_vibfocus(channel, vibSignal, sigLevel):
    """Check vibrometer and focus

    Checks focus of vibrometer sensor head and autofocuses if less
    then sigLevel specified (0 to ~1.1)

    :param channel: channel "signal" from polytec controller is
                    connected to on oscilloscope card
    """

    vibSignal.start_capture()
    vibSignal.readData(channel)
    signal = vibSignal.getDataRecordWise(channel)
    signal = np.average(signal, 0)

    k = 0
    while signal < sigLevel:
        print('sub-optimal focus:')
        if k == 0:
            Polytec().autofocusVibrometer(span='Small')
        elif k == 1:
            Polytec().autofocusVibrometer(span='Medium')
        else:
            Polytec().autofocusVibrometer(span='Full')
            vibSignal.start_capture()
            vibSignal.readData()
            signal = vibSignal.getDataRecordWise(channel)
            signal = np.average(signal, 0)
        k += 1
        if k > 3:
            print('unable to obtain optimum signal')
            break

        return signal

def plot(header, times, average, par):
    """ plot trace """
    plt.plot(times*1e6, average*header.calib)
    plt.xlim((0, max(times)*1e6))
    if header.calib_unit.rstrip() == 'nm/V':
        plt.ylabel('Displacement (nm)')
    elif header.calib_unit.rstrip() == 'mm/s/V':
        plt.ylabel('Particle Velocity (mm/s)')
    plt.xlabel('Time (us)')

def update_two_plot(times, average, x_value, par, header, fig, axis_1, axis_2):
    """Plot single trace and cumulative wavefield"""
    plt_data = read(par['FILENAME'], 'H5', calib=True)

    if par['GROUP_NAME_1'] in ['LONG_STAGE', 'SHORT_STAGE', 'PICOMOTOR-X', 'PICOMOTOR-Y']:
        plt_data.sort(keys=['x_position'])
        axis_2.set_ylabel('Scan Location ('+ header.x_unit + ')')
    elif par['GROUP_NAME_1'] == 'ROT_STAGE':
        plt_data.sort(keys=['theta_position'])
        axis_2.set_ylabel('Scan Location ('+ header.theta_unit + ')')

    axis_1.cla()
    axis_2.cla()
    axis_1.plot(times*1e6, average*header.calib)
    axis_1.set_xlim((0, max(times)*1e6))
    axis_2.imshow(
        plt_data,
        extent=[0, max(times)*1e6, x_value, par['I1']],
        cmap=par['MAP'],
        aspect='auto',
        )
    axis_1.set_xlabel('Time (us)')

    if header.calib_unit.rstrip() == 'nm/V':
        axis_1.set_ylabel('Displacement (nm)')
    elif header.calib_unit.rstrip() == 'mm/s/V':
        axis_1.set_ylabel('Particle Velocity (mm/s)')

    axis_2.set_xlabel('Time (us)')
    axis_1.set_xlim((0, max(times)*1e6))
    fig.canvas.draw()

def close(instruments, par):
    for device in instruments:
        if device == 'POLYTEC':
            Polytec().closeConnection()
        if device == 'INDI':
            QuantaRay().set(cmd='SING') # trn laser to single shot
            QuantaRay().off()
            QuantaRay().closeConnection()
        if device in ['PICOMOTOR-X', 'PICOMOTOR-Y']:
            PMot().close()
        if par['DIMENSIONS'] == 1 and device in ['SHORT_STAGE', 'LONG_STAGE', 'ROT_STAGE']:
            par['XPS_1'].TCP__CloseSocket(par['SOCKET_ID_1'])
            print('Connection to %s closed'%par['GROUP_NAME_1'])
        if par['DIMENSIONS'] == 2 and device in ['SHORT_STAGE', 'LONG_STAGE', 'ROT_STAGE']:
            par['XPS_2'].TCP__CloseSocket(par['SOCKET_ID_2'])
            print('Connection to %s closed'%par['GROUP_NAME_2'])

def point(par, header):
    """Record a single trace"""

    print('recording trace...')

    times, header = get_times(par['CONTROL'], par['CHANNEL'], header)

    update_header(header, par['I1'])

    if par['SOURCE'] == 'indi':
        laser_check = input('Turn laser on REP? (yes/N) \n')
        if laser_check == 'yes':
            QuantaRay().set('REP')
            sleep(1)
            QuantaRay().getStatus() # keep watchdog happy
        else:
            print('Turning laser off ...')
            QuantaRay().off()
            QuantaRay().closeConnection()
            # add code to close connection to instruments
            exit()

    # capture data
    average, average2 = data_capture(par)

    if par['PLOT'] is True:
        plot(header, times, average, par)
        if par['CHANNEL2'] != 'null' and par['RECEIVER'] != 'osldv':
            plot(header, times, average2, par)
        plt.show()

    # save data
    save_trace(header, average, par['FILENAME'])

    if par['CHANNEL2'] != 'null' and par['RECEIVER'] != 'osldv':
        save_trace(header, average2, par['FILENAME2'])

    if par['SOURCE'] == 'indi':
        QuantaRay().set('SING')
        QuantaRay().off()

    print('Trace recorded!')
    print('data saved as: %s \n '%par['FILENAME'])

def oneD(par, header):
    """Scanning function for 1-stage scanning"""

    #QSW().set(cmd='REP') # turn laser on repetitive shots
    #QRstatus().getStatus() # send command to laser to keep watchdog happy

    print('beginning 1D scan...')

    times, header = get_times(par['CONTROL'], par['CHANNEL'], header)

    if par['SOURCE'] == 'indi':
        laser_check = input('Turn laser on REP? (yes/N) \n')
        if laser_check == 'yes':
            QuantaRay().set('REP')
            sleep(1)
            QuantaRay().getStatus() # keep watchdog happy
        else:
            print('Turning laser off ...')
            QuantaRay().off()
            QuantaRay().closeConnection()
            # add code to close connection to instruments

    tracenum = 0
    if par['I1'] > par['F1']:
        par['D1'] = -par['D1']

    x_value = par['I1']

#unused        total_time = par['TOTAL_TIME']

    if par['GROUP_NAME_1'] == 'ROT_STAGE':
        pos = par['I1']
        unit = 'degrees'

    # set up mirrors
    elif par['GROUP_NAME_1'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']:
        theta_step = 1.8e-6 # 1 step = 1.8 urad
        print('Go to starting position for picomotors')
        PMot().Position(par['PX'], par['PY'])
        # set position to 'zero'
        PMot().set_DH(par['PX'])
        PMot().set_DH(par['PY'])
        if par['RECEIVER'] == 'polytec' or par['RECEIVER2'] == 'polytec':
            Polytec().autofocusVibrometer(span='Full')
            l_value = par['MIRROR_DISTANCE']
            unit = 'mm'
        else:
            l_value = par['MIRROR_DISTANCE']
            unit = 'radians'
        par['I1'] = float(par['I1'])/(l_value*theta_step)
        par['D1'] = float(par['D1'])/(l_value*theta_step)
        print('group name 1 %s' %par['GROUP_NAME_1'])
        if par['GROUP_NAME_1'] == 'PICOMOTOR-X':
            PMot().move_rel(par['PX'], par['I1'])
        else:
            PMot().move_rel(par['PY'], par['I1'])
    else:
        unit = 'mm'

    # setup plot
    axis_1, axis_2, fig = two_plot(par['GROUP_NAME_1'], header)
    i = 0

    while i < par['TOTAL_TRACES_D1']:
        if par['SOURCE'] == 'indi':
            QuantaRay().getStatus() # keep watchdog happy
        tracenum += 1
        print('trace ', tracenum, ' of', par['TOTAL_TRACES_D1'])

        # move stage/mirror
        if par['GROUP_NAME_1'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']:
#unused                x_steps = x_value/theta_step
            if par['GROUP_NAME_1'] == 'PICOMOTOR-X':
                PMot().move_rel(par['PX'], par['D1'])
                pos = float(PMot().get_TP(par['PX']))*l_value*theta_step
            elif par['GROUP_NAME_1'] == 'PICOMOTOR-Y':
                PMot().move_rel(par['PY'], par['D1'])
                pos = float(PMot().get_TP(par['PY']))*l_value*theta_step
        else:
            move_stage(
                par['GROUP_NAME_1'],
                par['XPS_1'],
                par['SOCKET_ID_1'],
                x_value
                )
            pos = x_value

        update_header(header, pos, par['GROUP_NAME_1'])
        print('position = {} {}'.format(pos, unit))
        sleep(par['WAITTIME']) # delay after stage movement

        #check_vibfocus(par['CHANNEL'],par['VIB_SIGNAL'],par['SIGNAL_LEVEL'])

        average, average2 = data_capture(par)

        # save current trace
        save_trace(header, average, par['FILENAME'])

        if par['CHANNEL2'] != 'null' and par['RECEIVER'] != 'osldv':
            save_trace(header, average2, par['FILENAME2'])

        # update figure
        if par['MAP'] != 'none' and i > 0:
            update_two_plot(times, average, x_value, par, header, fig, axis_1, axis_2)

        update_time(par)

        x_value += par['D1']
        i += 1

        #QRstatus().getStatus() # send command to laser to keep watchdog happy

        if par['RETURN'] == 'True':
            if par['GROUP_NAME_1'] == 'PICOMOTOR-X':
                PMot().move_abs(par['PX'], 0)
                print('picomotors moved back to zero.')
            elif par['GROUP_NAME_1'] == 'PICOMOTOR-Y':
                PMot().move_abs(par['PY'], 0)
                print('picomotors moved back to zero.')

    if par['SOURCE'] == 'indi':
        QuantaRay().set('SING')
        QuantaRay().off()
    print('scan complete!')
    print('data saved as: %s \n'%par['FILENAME'])

def twoD(par, header):
    """Scanning function for scanning with two stages."""

    print('beginning 2D scan...')

    _, header = get_times(par['CONTROL'], par['CHANNEL'], header)

    if par['SOURCE'] == 'indi':
        laser_check = input('Turn laser on REP? (yes/N) \n')
        if laser_check == 'yes':
            QuantaRay().set('REP')
            sleep(1)
            QuantaRay().getStatus() # keep watchdog happy
        else:
            print('Turning laser off ...')
            QuantaRay().off()
            QuantaRay().closeConnection()
            # add code to close connection to instruments
    tracenum = 0

    if par['I1'] > par['F1']:
        par['D1'] = -par['D1']
    x_value = par['I1']

    if par['I2'] > par['F2']:
        par['D2'] = -par['D2']
    y_value = par['I2']

#unused        total_time = par['TOTAL_TIME']

    # set up mirrors
    if (par['GROUP_NAME_1'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']
            or par['GROUP_NAME_2'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']):
        theta_step = 2.265e-6 # 1 step or count = 26 urad
        print('Go to starting position for picomotors')
        PMot().Position(par['PX'], par['PY'])
        print('done moving')
        # set current position to zero/home
        PMot().set_DH(par['PX'])
        PMot().set_DH(par['PY'])

    if par['GROUP_NAME_1'] == 'ROT_STAGE':
        unit1 = 'degrees'
    elif par['GROUP_NAME_1'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']:
        if par['RECEIVER'] == 'polytec':
            Polytec().autofocusVibrometer(span='Full')
            l_value = par['MIRROR_DISTANCE']
            unit1 = 'mm'
        else:
            l_value = par['MIRROR_DISTANCE']
            unit1 = 'radians'
        pos1 = 0
        par['I1'] = par['I1']/(l_value*theta_step)
        par['D1'] = par['D1']/(l_value*theta_step)

    else:
        unit1 = 'mm'

    if par['GROUP_NAME_2'] == 'ROT_STAGE':
        unit2 = 'degrees'
    elif par['GROUP_NAME_2'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']:
        if par['RECEIVER'] == 'polytec':
            Polytec().autofocusVibrometer(span='Full')
            l_value = par['MIRROR_DISTANCE']
            unit2 = 'mm'
        else:
            l_value = par['MIRROR_DISTANCE']
            unit2 = 'radians'
        pos2 = 0
        par['I2'] = par['I2']/(l_value*theta_step)
        par['D2'] = par['D2']/(l_value*theta_step)

    else:
        unit2 = 'mm'

    if par['GROUP_NAME_1'] in ['SHORT_STAGE', 'LONG_STAGE', 'ROT_STAGE']:
        pos1 = move_stage(
            par['GROUP_NAME_1'],
            par['XPS_1'],
            par['SOCKET_ID_1'],
            x_value,
            )
    if par['GROUP_NAME_2'] in ['SHORT_STAGE', 'LONG_STAGE', 'ROT_STAGE']:
        pos2 = move_stage(
            par['GROUP_NAME_2'],
            par['XPS_2'],
            par['SOCKET_ID_2'],
            y_value,
            )

    i = 0
    j = 0

    while i < par['TOTAL_TRACES_D1']:
        if par['SOURCE'] == 'indi':
            QuantaRay().getStatus() # keep watchdog happy
        print('trace {} of {}'.format(tracenum, par['TOTAL_TRACES_D1']*par['TOTAL_TRACES_D2']))

        if i > 0:
            if par['GROUP_NAME_1'] == 'PICOMOTOR-X':
                PMot().move_rel(par['PX'], par['D1'])
                pos1 = float(PMot().get_TP(par['PX']))*l_value*theta_step
            elif par['GROUP_NAME_1'] == 'PICOMOTOR-Y':
                PMot().move_rel(par['PY'], par['D1'])
                pos1 = float(PMot().get_TP(par['PY']))*l_value*theta_step
            else:
                pos1 = move_stage(
                    par['GROUP_NAME_1'],
                    par['XPS_1'],
                    par['SOCKET_ID_1'],
                    x_value,
                    )

        update_header(header, pos1 , par['GROUP_NAME_1'])

        print('dimension 1 = %s %s ' %(pos1, unit1))

        sleep(par['WAITTIME']) # delay after stage movement
        Polytec().autofocusVibrometer(span='Small')

        while j < par['TOTAL_TRACES_D2']:

            if par['SOURCE'] == 'indi':
                QuantaRay().getStatus() # keep watchdog happy

            tracenum += 1
            print('trace %s of %s' %(tracenum, par['TOTAL_TRACES_D1']*par['TOTAL_TRACES_D2']))

            if j > 0:
                if par['GROUP_NAME_2'] == 'PICOMOTOR-X':
                    PMot().move_rel(par['PX'], par['D2'])
                    pos2 = float(PMot().get_TP(par['PX']))*l_value*theta_step
                elif par['GROUP_NAME_2'] == 'PICOMOTOR-Y':
                    PMot().move_rel(par['PY'], par['D2'])
                    pos2 = float(PMot().get_TP(par['PY']))*l_value*theta_step
                else:
                    pos2 = move_stage(
                        par['GROUP_NAME_2'],
                        par['XPS_2'],
                        par['SOCKET_ID_2'],
                        y_value,
                        )

            update_header(header, pos2, par['GROUP_NAME_2'])

            print('dimension 2 = %s %s '%(pos2, unit2))

            sleep(par['WAITTIME']) # delay after stage movement

            #check_vibfocus(par['CHANNEL'],par['VIB_SIGNAL'],par['SIGNAL_LEVEL'])
            #Polytec().autofocusVibrometer(span='Small')

            average, average2 = data_capture(par)#par['CONTROL'],par['CHANNEL'])

            # save current trace
            save_trace(header, average, par['FILENAME'])

            if par['CHANNEL2'] != 'null' and par['RECEIVER'] != 'osldv':
                save_trace(header, average2, par['FILENAME2'])

            update_time(par)

            y_value += par['D2']
            j += 1

        x_value += par['D1']

        # move stage/mirror to starting position
        y_value = par['I2']

        if par['GROUP_NAME_2'] == 'PICOMOTOR-X':
            PMot().move_abs(par['PX'], float(y_value))
            #PMot().set_OR(par['PX'])
            pos2 = float(PMot().get_TP(par['PX']))*l_value*theta_step
        elif par['GROUP_NAME_2'] == 'PICOMOTOR-Y':
            #PMot().set_OR(par['PY'])
            PMot().move_abs(par['PY'], float(y_value))
            pos2 = float(PMot().get_TP(par['PY']))*l_value*theta_step
        else:
            pos2 = move_stage(
                par['GROUP_NAME_2'],
                par['XPS_2'],
                par['SOCKET_ID_2'],
                y_value
                )
        j = 0
        i += 1

    if par['SOURCE'] == 'indi':
        QuantaRay().set('SING')
        QuantaRay().off()

    print('scan complete!')
    print('data saved as: {} \n'.format(par['FILENAME']))

def dual(par, header):
    """Scanning function for 2-stage scanning.

    Scanning function for 2-stage scanning where both stages move at
    the same time.
    """

    print('beginning 2D scan...')

    times, header = get_times(par['CONTROL'], par['CHANNEL'], header)

    tracenum = 0

    if par['I1'] > par['F1']:
        par['D1'] = -par['D1']
    x_value = par['I1']

    if par['I2'] > par['F2']:
        par['D2'] = -par['D2']
    y_value = par['I2']

#unused        total_time = par['TOTAL_TIME']

    # set up mirrors
    if (par['GROUP_NAME_1'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']
            or par['GROUP_NAME_2'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']):
        theta_step = 2.265e-6 # 1 step or count = 26 urad
        print('Go to starting position for picomotors')
        PMot().Position(par['PX'], par['PY'])
        print('done moving')
        # set current position to zero/home
        PMot().set_DH(par['PX'])
        PMot().set_DH(par['PY'])

    if par['GROUP_NAME_1'] == 'ROT_STAGE':
        unit1 = 'degrees'
    elif par['GROUP_NAME_1'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']:
        if par['RECEIVER'] == 'polytec':
            Polytec().autofocusVibrometer(span='Full')
            l_value = par['MIRROR_DISTANCE']
            unit1 = 'mm'
        else:
            l_value = par['MIRROR_DISTANCE']
            unit1 = 'radians'
        pos1 = 0
        par['I1'] = par['I1']/(l_value*theta_step)
        par['D1'] = par['D1']/(l_value*theta_step)

    else:
        unit1 = 'mm'

    if par['GROUP_NAME_2'] == 'ROT_STAGE':
        unit2 = 'degrees'
    elif par['GROUP_NAME_2'] in ['PICOMOTOR-X', 'PICOMOTOR-Y']:
        if par['RECEIVER'] == 'polytec':
            Polytec().autofocusVibrometer(span='Full')
            l_value = par['MIRROR_DISTANCE']
            unit2 = 'mm'
        else:
            l_value = par['MIRROR_DISTANCE']
            unit2 = 'radians'
        pos2 = 0
        par['I2'] = par['I2']/(l_value*theta_step)
        par['D2'] = par['D2']/(l_value*theta_step)

    else:
        unit2 = 'mm'

    if par['GROUP_NAME_1'] in ['SHORT_STAGE', 'LONG_STAGE', 'ROT_STAGE']:
        pos1 = move_stage(
            par['GROUP_NAME_1'],
            par['XPS_1'],
            par['SOCKET_ID_1'],
            x_value,
            )
    if par['GROUP_NAME_2'] in ['SHORT_STAGE', 'LONG_STAGE', 'ROT_STAGE']:
        pos2 = move_stage(
            par['GROUP_NAME_2'],
            par['XPS_2'],
            par['SOCKET_ID_2'],
            y_value,
            )

    axis_1, axis_2, fig = two_plot(par['GROUP_NAME_1'], header)
    i = 0

    while i < par['TOTAL_TRACES_D1']:

        print('trace %s of %s' %(tracenum, par['TOTAL_TRACES_D1']))

        if i > 0:
            if par['GROUP_NAME_1'] == 'PICOMOTOR-X':
                PMot().move_rel(par['PX'], par['D1'])
                pos1 = float(PMot().get_TP(par['PX']))*l_value*theta_step
            elif par['GROUP_NAME_1'] == 'PICOMOTOR-Y':
                PMot().move_rel(par['PY'], par['D1'])
                pos1 = float(PMot().get_TP(par['PY']))*l_value*theta_step
            else:
                pos1 = move_stage(
                    par['GROUP_NAME_1'],
                    par['XPS_1'],
                    par['SOCKET_ID_1'],
                    x_value,
                    )

            if par['GROUP_NAME_2'] == 'PICOMOTOR-X':
                PMot().move_rel(par['PX'], par['D2'])
                pos2 = float(PMot().get_TP(par['PX']))*l_value*theta_step
            elif par['GROUP_NAME_2'] == 'PICOMOTOR-Y':
                PMot().move_rel(par['PY'], par['D2'])
                pos2 = float(PMot().get_TP(par['PY']))*l_value*theta_step
            else:
                pos2 = move_stage(
                    par['GROUP_NAME_2'],
                    par['XPS_2'],
                    par['SOCKET_ID_2'],
                    y_value,
                    )

        update_header(header, pos1, par['GROUP_NAME_1'])
        update_header(header, pos2, par['GROUP_NAME_2'])

        print('dimension 1 = {} {}'.format(pos1, unit1))
        print('dimension 2 = {} {}'.format(pos2, unit2))

        sleep(par['WAITTIME']) # delay after stage movement

        #check_vibfocus(par['CHANNEL'],par['VIB_SIGNAL'],par['SIGNAL_LEVEL'])
        #Polytec().autofocusVibrometer(span='Small')
        average, average2 = data_capture(par)#['CONTROL'],par['CHANNEL'])

        # save current trace
        save_trace(header, average, par['FILENAME'])

        if par['CHANNEL2'] != 'null':
            save_trace(header, average2, par['FILENAME2'])

        # update figure
        if par['MAP'] != 'none' and i > 0:
            update_two_plot(times, average, x_value, par, header, fig, axis_1, axis_2)

        update_time(par)

        y_value += par['D2']
        x_value += par['D1']
        i += 1
        tracenum += 1

    # move stages back to starting position
    x_value = par['I1']
    y_value = par['I2']

    if par['GROUP_NAME_2'] == 'PICOMOTOR-X':
        PMot().move_abs(par['PX'], float(y_value))
        pos2 = float(PMot().get_TP(par['PX']))*l_value*theta_step
    elif par['GROUP_NAME_2'] == 'PICOMOTOR-Y':
        PMot().move_abs(par['PY'], float(y_value))
        pos2 = float(PMot().get_TP(par['PY']))*l_value*theta_step
    else:
        pos2 = move_stage(
            par['GROUP_NAME_2'],
            par['XPS_2'],
            par['SOCKET_ID_2'],
            y_value,
            )

    if par['GROUP_NAME_1'] == 'PICOMOTOR-X':
        PMot().move_abs(par['PX'], float(x_value))
        pos1 = float(PMot().get_TP(par['PX']))*l_value*theta_step
    elif par['GROUP_NAME_1'] == 'PICOMOTOR-Y':
        PMot().move_abs(par['PY'], float(x_value))
        pos1 = float(PMot().get_TP(par['PY']))*l_value*theta_step
    else:
        pos1 = move_stage(
            par['GROUP_NAME_1'],
            par['XPS_1'],
            par['SOCKET_ID_1'],
            x_value,
            )

    # finish.
    print('scan complete!')
    print('data saved as: %s \n'%par['FILENAME'])