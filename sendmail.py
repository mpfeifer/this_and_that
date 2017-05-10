#!/usr/bin/python3

# Import smtplib for the actual sending function
import smtplib
# Import the email modules we'll need
from email.mime.text import MIMEText
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

    name               = 'sendmail.py'
    version            = '0.1'
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
        parser.add_argument("-d", "--logdomain", help="Domain for logging", default="py.sendmail")
        parser.add_argument("-r", "--remotelog", help="Enable remote logging with default host and port", action="store_true")
        parser.add_argument("-f", "--From", help="Set sender address", default = "map@compumatica.com")
        parser.add_argument("-t", "--To", help="Set receiver address", default = "matthias.pfeifer@compumatica.com")
        parser.add_argument("-s", "--server", help="Set smtp server address", type = str)
        parser.add_argument("-b", "--body", help = "File with e-Mail body content", type = str)
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
        msg = None
        self.log.debug('Reading mail body from file {}'.format(self.args.body))
        with open(self.args.body) as fp:
            msg = MIMEText(fp.read())

        msg['Subject'] = 'The contents of %s' % self.args.body
        self.log.debug('Sending mail from {}'.format(self.args.From))
        msg['From'] = self.args.From
        self.log.debug('Sending mail to {}'.format(self.args.To))
        msg['To'] = self.args.To
        self.log.debug('Using server {}'.format(self.args.server))
        s = smtplib.SMTP(self.args.server)
        try:
            s.sendmail(self.args.From, [self.args.To], msg.as_string())
        except e:
            self.log.error(e)
        s.quit()

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
