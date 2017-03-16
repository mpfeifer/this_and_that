#!/usr/bin/python3

import logging
import logging.handlers
import signal
import sys
import os
import time
import argparse

# Author     : Matthias
# Description: Python script template

class Application:

    name               = 'gen-analyzer.py3'
    logdomain          = 'log.analyzer'
    version            = '1.0'
    log                = None
    properties         = None
    parser             = None
    args               = None

    def __init__(self):
        signal.signal(signal.SIGINT, Application.signal_int_handler)        
        parser = argparse.ArgumentParser(description="", epilog="")
        parser.add_argument("-v", "--verbose", help="Be more verbose when logging", action="store_true")
        parser.add_argument("-P", "--properties", help="A properties file for use by the application", type=str)
        parser.add_argument("-l", "--loghost", help="Name of host to receive log messages", default="127.0.0.1")
        parser.add_argument("-p", "--logport", help="Port of service to receive log messages", type=int, default=logging.handlers.DEFAULT_TCP_LOGGING_PORT)
        parser.add_argument("-d", "--logdomain", help="Domain for logging", default=self.logomain)
        parser.add_argument("-r", "--remotelog", help="Enable remote logging with default host and port", action="store_true")
        parser.add_argument("-f", "--file", help="Give name of logfile to analyze", type=str)
        parser.add_argument("-o", "--output", help="Name of hmlt file that will be created", type=str)        
        self.args = parser.parse_args()
        self.parser = parser
        self.setup_logging()
        self.read_properties(self.args.properties)
        
    def setup_logging(self):
        """ Setup logging so that a root logger is configured with formatter and handler
        according to configuration. Additional loggers should just propagate to the root
        logger. """
        self.log = logging.getLogger(self.args.logdomain)
        rootlogger = logging.getLogger()
        formatstring='%(asctime)s %(levelname)-15s %(name)s # %(message)s'
        formatter = logging.Formatter(fmt=formatstring, datefmt='%d.%m.%y %I:%M:%S')
        handler = None
        if self.args.remotelog:
            handler = logging.handlers.SocketHandler(self.loghost_name, self.loghost_port)
        else:
            handler = logging.StreamHandler(sys.stderr)
        handler.setFormatter(formatter)
        rootlogger.addHandler(handler)
        level = logging.INFO
        if self.args.verbose:
            level = logging.DEBUG
        self.log.setLevel(level)
        rootlogger.setLevel(level)
        self.log.propagate=1
        
    def read_properties(self, filename):
        """ Treat the file with given filename as a properties file. """
        if filename:
            properties = {}
            comment_char = "#"
            seperator = ":"
            with open(filename, "rt") as f:
                for line in f:
                    l = line.strip()
                    if l and not l.startswith(comment_char):
                        key_value = l.split(seperator)
                        key = key_value[0].strip()
                        value = seperator.join(key_value[1:]).strip()
                        properties[key] = value 
            self.properties = properties

    @staticmethod
    def signal_int_handler(signal, frame):
        interrupt_msg = '\r\n\r\n{} {} terminated by keyboard interrupt'.format(Application.name, Application.version)
        print(interrupt_msg)
        exit(0)

    def run(self):
        result = "<html>\r\n"
        result += "<head>\r\n"
        result += "<title></title>\r\n"
        result += "<script src = \"jquery-3.1.0.js\"></script>\r\n";
        result += "<script src = \"loganalyzer.js\"></script>\r\n";
        result += "<link rel=\"stylesheet\" href=\"loganalyzer.css\" />\r\n"
        result += "</head>\r\n"
        result += "<body>\r\n"
        self.log.info('Reading input file {}'.format(self.args.file))
        processHeader = True
        toggle = True
        with open(self.args.file) as f:
            for line in f:
                line = line.rstrip()
                ld = line.split()
                if processHeader:
                    result += "<div class = \"logline-header\">"
                    result += "<span class = \"date\"> DATE </span>"
                    result += "<span class = \"time\"> TIME </span>"
                    result += "<span class = \"evnt\"> EVNT </span>"
                    result += "<span class = \"text\"> TEXT </span>"
                    result += "</div>\r\n"
                    processHeader = False
                    continue
                if len(ld) > 4 :
                    self.log.debug("Processing line: {}".format(line.rstrip()))

                    if toggle:
                        result += "<div class = \"logline-light\">"
                        toggle = not toggle
                    else:
                        result += "<div class = \"logline-dark\">"
                        toggle = not toggle

                    result += "<span class = \"date\">" + ld[0] + "</span>"
                    result += "<span class = \"time\">" + ld[1] + "</span>"
                    result += "<span class = \"evnt\">" + ld[2] + " " + ld[3] + "</span>"
                    result += "<span class = \"text\">" + " ".join(ld[4:]) + "</span>"
                    result += "</div>\r\n"

                else:
                    self.log.debug("Processing line: {}".format(line.rstrip()))
                    toggle = not toggle
                    if toggle:
                        result += "<div class = \"logline-light\">"
                        toggle = not toggle
                    else:
                        result += "<div class = \"logline-dark\">"
                        toggle = not toggle
                    result += "<span class = \"text-amendment\">" + " ".join(ld[0:]) + "</span>"
                    result += "</div>\r\n"
        result += "</body>\r\n"
        self.log.info('Writing output file {}'.format(self.args.output))
        target = open(self.args.output, 'w')
        target.write(result)
        target.close()

def main():
    app = Application()
    app.log.info('{} {} is starting'.format(app.name, app.version))
    app.run()
    app.log.info('{} {} is done'.format(app.name, app.version))

if __name__ == '__main__':
    main()

#
# Done
#
# # # end of script
