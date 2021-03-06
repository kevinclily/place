PLACE Glossary
==============

.. glossary::
   :sorted:

   binary format
     Binary files are not stored using characters from any readable language
     and are instead written in mathematical values stored as bytes. To read
     the data in a binary file, you generally need use a program which
     understands the binary format of the file. PLACE saves files in the NumPy
     binary format, meaning it can be read using the numpy library in Python.

   command
     Elm uses commands to complete actions safely. Things like writing data to
     disk or sending information over a port can fail, so Elm handles these
     things for us. This doesn't mean these things cannot fail, it just means
     that Elm will simplify the process of checking for failure by doing most
     of the checking for us. `Read more <http://package.elm-lang.org/packages/elm-lang/core/latest/Platform-Cmd>`_

   experiment
     Within the PLACE context, an *experiment* refers to any execution of the
     PLACE software. In older versions of PLACE, this may also be referred to
     as a *scan*.

   PLACE
     Python Laboratory Automation, Control, and Experimentation.

   message
     Elm uses messages to communicate changes made by the user, along with some
     other internal changes.  So, for example, when the user picks a new option
     in a dropdown menu, a message is generated in the code to update the model
     to reflect the user's change.

   metadata
     Data about data. This is used by PLACE to describe the values contained in
     the NumPy data file produced by PLACE. It will contain the original configuration
     options used to start the experiment along with other information.

   module
     Refers to an independent group of files which instruct PLACE how to
     interact with specific hardware. Typically, this refers to both a Python
     backend interface with PLACE and a JavaScript frontend interface with the
     user. PLACE supports dynamic interaction with properly written modules.

   priority
     All PLACE modules are given a priority value which determines their order
     of execution during an experiment.  Instruments with lower values are
     executed earlier in the rotation than those with higher values. Arguments
     can be made that this is backwards, but that would still be true if the
     order was reversed. Instruments with the same priority are not executed in
     parallel (yet), and PLACE with just select one to go first.

   scan
     Legacy term. See :term:`experiment`.
