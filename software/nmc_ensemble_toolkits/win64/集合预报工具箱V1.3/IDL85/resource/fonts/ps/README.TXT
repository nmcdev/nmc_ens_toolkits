------------------------------------------------------------
README file: <IDL_Directory>
               resource
                 fonts
                   ps
------------------------------------------------------------

This subdirectory contains files with support the IDL PostScript
graphics driver:

   ehandler.ps - PostScript Downloaded Error Break-page handler from
	Adobe Systems Incorporated. Normally, any errors in a PostScript
	file cause the printer to silently kill the job. Sending this
	file to your PostScript printer causes it to print any subsequent
	errors it encounters on a sheet of paper and eject it. The effect
	of this file lasts until the printer is reset.

	Clearly, a workstation with a PostScript previewer is a
	better way to debug PostScript, but if you only have a printer,
	this is the way to go.

   prolog.ps - The PostScript prolog file. This file contains
	standard PostScript definitions used by all IDL generated
	PostScript files. IDL includes a copy of this file at the top
	of every generated output file.

   prheader.ps - A PostScript header file used by the UNIX
   IDLDE when printing code from an editor window. Includes
   PostScript commands to print the filename, date, and other
   information on the output pages.

   Adobe Font Metric (.afm) files - These files supply font
	metric information which is used by IDL to properly position
	text output.


   font.map - A text file which maps PostScript font names to the
	name of their corresponding afm file. This approach avoids
	file name length problems that occur on some operating systems.


----------------------------------------
To add an Adobe font metric file to IDL:
----------------------------------------

    1) IDL doesn't use standard Adobe AFM files directly. Rather, it uses
       the output of a user library procedure named PSAFM. PSAFM
       takes a standard AFM file as input and creates as output
       an AFM file more to IDLs liking. Use PSAFM to convert
       the Adobe AFM file and place the result in this directory.
       Choose a name of 8 or fewer characters with a ".afm"
       extension. Make sure this name doesn't already appear in
       font.map. Usually the name of the original AFM file is fine:

	Example:

        IDL> PSAFM, '/home/user/newafm.afm', $
	     '/usr/local/itt/idl/resource/fonts/ps/newafm.afm'

    2) Use a standard text editor to add an entry in font.map for the new
       AFM file. The field labeled "ISO encoding?" should be answered by
       looking in your new AFM file for the string "ISOLatin1Encoding.
       If this is found, enter a "1", otherwise enter a "0".

--------
Example:
--------
    My development system (Solaris 9) comes with a PostScript previewer
named GhostScript (gs) that can be used to examine PostScript output
from programs like IDL. The AFM files for the fonts it understands
can be found in /usr/openwin/lib/X11/fonts/F3/afm. This example will
show how to produce IDL PostScript output that uses the GillSans-BoldItalic
font, and then display the results using pageview. As shipped by
ITT Visual Information Solutions, GillSans-BoldItalic is not one of the
fonts that IDL understands by default:

    1) Run IDL and use PSAFM to generate the IDL afm file. On most
       systems, you will need root access to create the file in the
       IDL distribution tree:

	IDL>PSAFM,'/usr/openwin/lib/X11/fonts/F3/afm/GillSans-BoldItalic.afm',$
	    '/usr/local/itt/idl/resource/fonts/ps/GlS-BoI.afm'

    2) Edit the IDL font.map file in the resource/fonts/ps subdirectory of
       the IDL distribution and add the following line at the end:

	GillSans-BoldItalic    GlS-BoI    1

    3) Run IDL and create a plot:

	IDL> set_plot,'ps'
	IDL> device, set_font='GillSans-BoldItalic'
	IDL> plot,[0,1],title='Nonsense Plot',xtit='X',ytitle='Y', FONT=0
	IDL> exit

    4) View the results:

	% gs idl.ps
