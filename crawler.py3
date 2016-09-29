#!/usr/bin/python3

import logging
import logging.handlers
import signal
import sys
import os
import configparser
import getopt
import time
import html.parser
import urllib.request as req
import urllib.error



#Define HTML Parser
class hrefExtractingParse(html.parser.HTMLParser):

    def __init__(self):
        self.urlText=[]

    def handle_starttag(self, tag, data):
        if tag == 'a':
            for attrib in data:
                key = attrib[0]
                value = attrib[1]
                if key == "href":
                    self.urls.append(value)

class Application:
    name               = 'template'
    version            = '0.1'
    def __init__(self):
        self.loghost_port  = logging.handlers.DEFAULT_TCP_LOGGING_PORT
        self.loghost_name  = 'localhost'
        self.logdomain     = 'template.py'
        self.usage_string  = 'Usage: this-script.py [inifile] -h -v -p <numeric> -l <hostname> -r\r\n\r\n'
        self.usage_string+=' [inifile]  if inifile is set it is read before commandline switches\r\n'
        self.usage_string+='    -h      print usage string\r\n'
        self.usage_string+='    -l      set remote logging host (this enables remote logging)\r\n'
        self.usage_string+='    -p      set remote logging port (this enables remote logging)\r\n'
        self.usage_string+='    -r      enabled remote logging\r\n'
        self.usage_string+='    -v      be more verbose\r\n'
        self.usage_string+='\r\nIf remote logging is enabled in inifile it cannot be disabled via commandline.\r\n'
        self.usage_string+='{} version {}'.format(Application.name, Application.version)
        self.inifile       = None
        self.inifile_name  = 'undefined'
        self.remote_logger_enabled = False
        self.log_verbose = False
        self.gather_parameter()
        self.log           = self.get_logger()
        self.log_configuration()
        signal.signal(signal.SIGINT, Application.signal_int_handler)        


    def log_configuration(self):
        self.log.debug("%28s = %s", "Application.name", Application.name)
        self.log.debug("%28s = %s", "version", Application.version)
        self.log.debug("%28s = %s", "inifile_name", self.inifile_name)
        self.log.debug("%28s = %s", "log", self.log)
        self.log.debug("%28s = %s", "remote_logger_enabled", self.remote_logger_enabled)
        self.log.debug("%28s = %s", "loghost_port", self.loghost_port)
        self.log.debug("%28s = %s", "loghost_name", self.loghost_name)

        
    def get_logger(self):
        log = logging.getLogger(self.logdomain)
        formatstring='%(asctime)s %(levelname)-15s %(name)s # %(message)s'
        formatter = logging.Formatter(fmt=formatstring, datefmt='%d.%m.%y %I:%M:%S')
        handler = None
        if self.remote_logger_enabled:
            handler = logging.handlers.SocketHandler(self.loghost_name, self.loghost_port)
        else:
            handler = logging.StreamHandler(sys.stderr)
        handler.setFormatter(formatter)
        log.addHandler(handler)
        level = logging.INFO
        if self.log_verbose:
            level = logging.DEBUG
        log.setLevel(level)
        log.propagate=0
        return log

        
    def gather_parameter(self):
        '''Collect parameters from inifile (first) and then from commandline.'''

        first_getopt_index=1

        if len(sys.argv)> 1 and not sys.argv[1].startswith('-'):
            self.inifile_name=sys.argv[1]
            first_getopt_index=2

        if os.path.isfile(self.inifile_name):
            self.inifile=configparser.ConfigParser()
            self.inifile.read(self.inifile_name)

        if self.inifile:
            self.loghost_name=self.inifile.get('logging', 'hostname') # , fallback=self.loghost_name)
            self.loghost_port=self.inifile.get('logging', 'port', fallback=self.loghost_port)
            if (self.inifile.getboolean('logging', 'remote_logging', fallback=False)):
                self.remote_logger_enabled=True

        try:
            opts, args = getopt.getopt(sys.argv[first_getopt_index:], 'vhp:l:r', 'help')
        except getop.GetoptError as err:
            print(err)
            self.usage()

        for option, arg in opts:
            if option == '-h':
                self.usage();
            elif option == '-p':
                self.loghost_port=int(arg)
                self.remote_logger_enabled = True
            elif option == '-l':
                self.loghost_name=argument
                self.remote_logger_enabled = True
            elif option == '-r':
                self.remote_logger_enabled = True
            elif option == '-v':
                self.log_verbose = True

    def usage(self):
        print(self.usage_string)
        exit(-1)

    @staticmethod
    def signal_int_handler(signal, frame):
        interrupt_msg = '\r\n\r\n{} {} terminated by keyboard interrupt'.format(Application.name, Application.version)
        print(interrupt_msg)
        exit(0)

    def run(self):
        lParser = None
        starturl = "http://www-rohan.sdsu.edu/~gawron/index.html"
        try:
            with req.urlopen(starturl) as response:
                html = response.read().decode("utf-8")
                lParser = hrefExtractingParse()
                lParser.feed(html)
                lParser.close()
        except urllib.error.HTTPError as e:
            print('Error code: ', e.code)
        except urllib.error.URLError as e:
            print('Reason: ', e.reason)

        for url in lParserl.urls:
            print(url)

def main():
    app = Application()
    app.log.info('{} {} will be instantiated'.format(app.name, app.version))
    app.run()
    app.log.info('{} {} is done'.format(app.name, app.version))

if __name__ == '__main__':
    main()

#
# Done
#
# # # end of script
