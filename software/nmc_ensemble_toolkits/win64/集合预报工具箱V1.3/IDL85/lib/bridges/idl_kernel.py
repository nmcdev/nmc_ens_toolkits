from IPython.kernel.zmq.kernelbase import Kernel
from idlpy import IDL

import collections
import re

import hashlib

__version__ = '1.0.0'

# -----------------------------------------------------------------------------


class IDL_kernel(Kernel):
    implementation = 'idl_kernel'
    implementation_version = __version__
    language = 'IDL'
    language_info = {
        'name': 'idl',
        'codemirror_mode': 'idl',
        'mimetype': 'text/x-idl',
        'file_extension': '.pro'
    }
    _banner = "IDL"
    _graphicid = '__IDLGRAPHICPLACEHOLDER__'
    codeComplete = None
    history = None


    # -------------------------------------------------------------------------
    @property
    def banner(self):
        return self._banner


    # -------------------------------------------------------------------------
    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self.idl_initGConfig()


    # -------------------------------------------------------------------------
    def idl_initGConfig(self):
        IDL.run("compile_opt idl2", silent=True)
        IDL.run("!QUIET=1 & !MAGIC.embed=1 & !MAGIC.window=-1", silent=True)


    # -------------------------------------------------------------------------
    def idl_resetGConfig(self):
        IDL.run("!MAGIC.window=-1", silent=True)


    # -------------------------------------------------------------------------
    def idl_do_execute(self, code, call_result, graphics):
        call_line = ''

        for each in code.splitlines():
            each = each.rstrip()
            if each.lower().startswith('.run'):
                continue

            if len(each) > 0 and each[-1] == '$':
                call_line += each[0:-1]
                continue
            else:
                call_line += each

            result = IDL.run(call_line)
            call_line = ''

            idlmagic = getattr(IDL, '!MAGIC')
            if idlmagic['WINDOW'] != -1 and idlmagic['WINDOW'] not in graphics:
                graphics[idlmagic['WINDOW']] = idlmagic
                call_result.append(self._graphicid)
            if result is not None:
                call_result.append(result)

            if '.res' in each.lower() or '.f' in each.lower():
                self.idl_initGConfig()


    # -------------------------------------------------------------------------
    def idl_output_result(self, result, graphics):

        # Check if PNG
        if result == self._graphicid and len(graphics) > 0:
            # Returns a tuple with the window ID and !magic info.
            idlmagic = graphics.popitem(False)[1]
            img = IDL.EncodeGraphic(idlmagic['WINDOW'], idlmagic['TYPE'])
            if img != None:
                # See Python logging module
#                self.log.warning("IDL graphic " + str(idlmagic['WINDOW']) + " md5=" +
#                    hashlib.md5(img.encode()).hexdigest())
                width = str(idlmagic['XSIZE'])
                height = str(idlmagic['YSIZE'])
                response = {
                    'data': { 'image/png': img },
                    'metadata': { 'image/png': {'width': width, 'height': height} }
                }
            else:
                err = 'Error: Unable to encode graphics window ' + str(idlmagic['WINDOW']) + '\n'
                response = {
                    'data': { 'text/plain': err },
                    'metadata': { }
                }
        else:
            if result.startswith("<html>"):
                response = {
                    'data': { 'text/html': result + '\n' },
                    'metadata': { }
                }
            else:
                response = {
                    'data': { 'text/plain': result + '\n' },
                    'metadata': { }
                }

        self.send_response(self.iopub_socket, 'display_data', response)


    # -------------------------------------------------------------------------
    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False):

        reply = {}

        try:
            call_result = []
            graphics = collections.OrderedDict()
            self.idl_do_execute(code, call_result, graphics)

        except:
            status = u'error'
            reply['ename'] = 'Unknown error'

        else:
            status = u'ok'
            if not silent:
                for line in call_result:
                    self.idl_output_result(line, graphics)


        self.idl_resetGConfig()

        reply[u'status'] = status
        reply['execution_count'] = self.execution_count - 1
        reply['payload'] = []

        return reply


    # -------------------------------------------------------------------------
    def processIDLCatalog(self, catFile):
        result = []
        # Python 3 requires the encoding keyword, but Python 2 doesn't support it,
        # so use a try/except to handle both cases.
        try:
            myfile = open(catFile, encoding='utf-8')
        except Exception:
            myfile = open(catFile)
        with myfile as catalogFile:
            while True:
                line = catalogFile.readline()
                if not line: break
                pos = line.find("<ROUTINE name=")
                if (pos >= 0):
                    routine = (line.split('"'))[1]
                    if ((len(routine) > 0) and (routine.upper() == routine)):
                        result.append(routine)
        result.sort()
        return result


    # -------------------------------------------------------------------------
    # Tab completion - reads in the IDL documentation catalog file,
    # parses out the routine list and caches it.
    # Then, when the user hits tab, it searches for all matches.
    #
    # This appears to be used by ipython qtconsole, console, and notebook.
    #
    def do_complete(self, code, cursor_pos):
        # See if we need to construct our cache
        if (IDL_kernel.codeComplete == None):
            idldir = getattr(IDL, '!DIR')
            idlCatalog = idldir + '/help/idl_catalog.xml'
            IDL_kernel.codeComplete = self.processIDLCatalog(idlCatalog)
        word = str(code[0:cursor_pos]).upper()
        # Naive regular expression for the last valid IDL identifier.
        word = (re.split('[ =+\-\*\/\(,]*', word))[-1]
        result = []
        cursor_start = cursor_pos
        if (len(word) > 0):
            for idlword in IDL_kernel.codeComplete:
                if idlword.startswith(word):
                    result.append(idlword)
            # Start the autocomplete at the beginning of the IDL identifier.
            if (len(result) > 0): cursor_start = cursor_pos - len(word)
        return {'status': 'ok', 'matches': result,
            'cursor_start': cursor_start, 'cursor_end': cursor_pos}


    # -------------------------------------------------------------------------
    # Is the command ready to execute, or does it need to continue
    # on the next line?
    #
    # This doesn't appear to be working with any of the ipython front ends.
    #
    def do_is_complete(self, code):
        status = {'status': 'complete'}
        if (code.endswith("$")):
            status = {'status': 'incomplete', 'indent': '  '}
        return status


    # -------------------------------------------------------------------------
    # This works with the qtconsole when you type a parentheses. For example:
    #    a = FFT(
    #
    def do_inspect(self, code, cursor_pos, detail_level=0):
        reply_content = {'status' : 'ok'}
        # This should contain the "docs" for the function call.
        result = {'text/plain': 'Not yet implemented...'}
        reply_content['data'] = result
        reply_content['metadata'] = {}
        # Change this to True when we implement this.
        reply_content['found'] = False
        return reply_content


    # -------------------------------------------------------------------------
    def processIDLHistory(self, histFile):
        result = []
        # Python 3 requires the encoding keyword, but Python 2 doesn't support it,
        # so use a try/except to handle both cases.
        try:
            myfile = open(histFile, encoding='utf-8')
        except Exception:
            myfile = open(histFile)
        with myfile as historyFile:
            index = 0
            commands = []
            while True:
                line = historyFile.readline()
                if not line: break
                line = line.rsplit(" <!--")
                if (len(line) >= 2):
                    result.append(line[0])
                    index = index + 1
            result.reverse()
            result = [(None, None, h) for h in result]
        return result


    # -------------------------------------------------------------------------
    # This is used by the ipython qtconsole, but does not appear to be used
    # for the ipython console or ipython notebook.
    #
    def do_history(self, hist_access_type, output, raw, session=None, start=None,
        stop=None, n=None, pattern=None, unique=False):
        # See if we need to construct our cache
        if (IDL_kernel.history == None):
            from os.path import expanduser
            idldir = getattr(IDL, '!DIR')
            idlHistory = expanduser("~") + '/.idl/idl/rbuf/history'
            IDL_kernel.history = self.processIDLHistory(idlHistory)
        if (hist_access_type == "tail"):
            history = self.history[max(0,len(self.history) - n):]
        return {'history': history }


    # -------------------------------------------------------------------------
    def do_interrupt(self, interrupt):
        self.log.debug("Interrupt IDL kernel...")
        return {'status': 'ok', 'interrupt': interrupt}


    # -------------------------------------------------------------------------
    def do_shutdown(self, restart):
        self.log.debug("Shutting down IDL kernel...")
        return {'status': 'ok', 'execution_count': self.execution_count, 'restart': restart}


# -----------------------------------------------------------------------------
if __name__ == '__main__':
    from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=IDL_kernel)
