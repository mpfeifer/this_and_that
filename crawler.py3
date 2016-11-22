#!/usr/bin/python3

import logging
import logging.handlers
import signal
import sys
import os
import configparser
import getopt
import time

from html.parser import HTMLParser

import urllib.request as req
import urllib.error

from urllib import parse
from urllib.request import urlopen

import hashlib
import sys
import re
import time

class CrawlerHtmlParser(HTMLParser):

    """HTML Parser extracts href attribute values from a tags."""

    tld_file_name = "tld.txt"
    tlds = None

    stopwords_file_name = "ignore.txt"
    stopwords = None

    def __init__(self):

        HTMLParser.__init__(self)

        self.log=logging.getLogger("crawler.parser")
        self.log.propagate = 1
        self.urls=[]
        self.url_text=[]
        self.base_url = None
        
        # This is calculated from url content
        self.histogram=None

        # This is taken from url meta tag
        self.keywords=None

        # This is taken from url meta tag        
        self.description=None

        # This is taken from url meta tag
        self.language=None

        self.timestamp=time.time() - time.altzone
        
        if CrawlerHtmlParser.tlds == None:
            CrawlerHtmlParser.tlds = []
            tld_file = open(CrawlerHtmlParser.tld_file_name, "r")
            for tld in tld_file:
                if not tld.startswith("#"):
                    CrawlerHtmlParser.tlds.append(tld.strip().lower())
            tld_file.close()
            self.log.debug("tld names were loaded: %s", CrawlerHtmlParser.tlds)

        if CrawlerHtmlParser.stopwords == None:
            CrawlerHtmlParser.stopwords = []
            stopwords_file = open(CrawlerHtmlParser.stopwords_file_name, "r")
            for word in stopwords_file:
                CrawlerHtmlParser.stopwords.append(word.strip().lower())
            stopwords_file.close()
            self.log.debug("Stopwords were loaded: %s", CrawlerHtmlParser.stopwords)

    def handle_data(self, data):
        processed_data=data.strip();
        if (len(processed_data) > 0):
            if processed_data.endswith("."):
                processed_data = processed_data[:-1]
            self.url_text.append(processed_data)
        
    def handle_starttag(self, tag, attributes):
        if tag == 'a':
            for (key, value) in attributes:
                if key == "href":
                    newUrl = parse.urljoin(self.base_url, value)
                    self.urls.append(newUrl)

    def getLinks(self, url):
        self.base_url = url
        response = urlopen(url)
        charset=self.guessCharset(response);
        if (charset):
            htmlBytes = response.read()
            try:
                htmlString = htmlBytes.decode(charset)
            except UnicodeDecodeError:
                htmlString = None
            self.feed(htmlString)
        if self.url_text:
            self.calculate_keywords()
        return self.urls

    def guessCharset(self, response):
        result=None
        content_type=response.getheader('Content-Type').lower()
        if (content_type == "text/html"):
            result="iso-8859-1"
        else:
            m = re.search('charset=(.*)', content_type)
            if (m.lastindex == 1):
                result=m.group(1)
        self.log.debug("Guessing charset for url {}: {}".format(self.base_url, result))
        return result

    def calculate_keywords(self):
        self.histogram = {}
        self.keywords = {}
        for phrase in self.url_text:
            words = re.split('\W+', phrase)
            for word in words:
                word=word.lower()
                if word in CrawlerHtmlParser.stopwords:
                    continue
                if word in self.histogram:
                    self.histogram[word]=self.histogram[word]+1
                else:
                    self.histogram[word]=1                    
        for current_key in self.histogram:
            if current_key in self.keywords:
                continue
            if len(self.keywords) >= 5:
                break
            current_value = self.histogram[current_key]
            max_key=current_key
            max_val=current_value
            for comp_key in self.histogram.keys():
                if comp_key in self.keywords or comp_key == current_key:
                    continue
                comp_val = self.histogram[comp_key]
                if (comp_val > max_val):
                    max_val = comp_val
                    max_key = comp_key
            if max_key:
                self.keywords[max_key]=max_val

        self.log.info("histogram = {}".format(self.histogram))
        self.log.info("Keywords = {}".format(self.keywords))
        
    def getKeywords(self):
        return self.keywords
    
    def getDescription(self):
        return self.description

    def getLanguage(self):
        return self.language
    
    def getTimestamp(self):
        return self.timestamp

class UrlExtract():
    """This class hosts the data, that was extracted from a url."""
    def __init__(self, hashval="", url="", keys=[], desc="", lang="", timestamp=None):
        self.hashval=hashval
        self.url=url
        self.keywords=keys
        self.description=desc
        self.language=lang
        self.timestamp=timestamp

    def copy_from_parser(self, parser):
        self.keywords=parser.getKeywords()
        self.description=parser.getDescription()
        self.language=parser.getLanguage()
        self.timestamp=parser.getTimestamp()
        
class Application:
    name               = 'web crawler'
    version            = '0.1'
    def __init__(self):
        self.starting_url  = "http://www.dmoz.org"
        self.loghost_port  = logging.handlers.DEFAULT_TCP_LOGGING_PORT
        self.loghost_name  = 'localhost'
        self.logdomain     = 'crawl'
        self.usage_string  = 'Usage: crawler.py3 [inifile] -h -v -p <numeric> -l <hostname> -u <url> -r\r\n\r\n'
        self.usage_string+=' [inifile]  if inifile is set it is read before commandline switches\r\n'
        self.usage_string+='    -h          print usage string\r\n'
        self.usage_string+='    -q filename be quiet and log to filename'
        self.usage_string+='    -l hostname set remote logging host (this enables remote logging)\r\n'
        self.usage_string+='    -p num      set remote logging port (this enables remote logging)\r\n'
        self.usage_string+='    -r          enabled remote logging\r\n'
        self.usage_string+='    -v          be more verbose\r\n'
        self.usage_string+='    -u url      starting url\r\n'
        self.usage_string+='\r\nIf remote logging is enabled in inifile it cannot be disabled via commandline.\r\n'
        self.usage_string+='{} version {}'.format(Application.name, Application.version)
        self.inifile      = None
        self.inifile_name = 'undefined'
        self.remote_logger_enabled = False
        self.log_verbose  = False
        self.logfile      = None
        self.gather_parameter()

        # setup logger
        self.log          = self.get_logger()

        self.log_configuration()

        signal.signal(signal.SIGINT, Application.signal_int_handler)        

    def log_configuration(self):
        self.log.info("%28s = %s", "Application.name", Application.name)
        self.log.info("%28s = %s", "version", Application.version)
        self.log.info("%28s = %s", "inifile_name", self.inifile_name)
        self.log.info("%28s = %s", "log", self.log)
        self.log.info("%28s = %s", "remote_logger_enabled", self.remote_logger_enabled)
        self.log.info("%28s = %s", "loghost_port", self.loghost_port)
        self.log.info("%28s = %s", "loghost_name", self.loghost_name)
        self.log.info("%28s = %s", "logfile", self.logfile)
        self.log.info("%28s = %s", "starting_url", self.starting_url)

    def get_logger(self):
        log = logging.getLogger(self.logdomain)
        root_logger =  logging.getLogger('')
        formatstring='%(asctime)s %(levelname)-15s %(name)s # %(message)s'
        formatter = logging.Formatter(fmt=formatstring, datefmt='%d.%m.%y %I:%M:%S')
        handler = None
        if self.remote_logger_enabled:
            handler = logging.handlers.SocketHandler(self.loghost_name, self.loghost_port)
        elif not self.logfile:
            handler = logging.StreamHandler(sys.stdout)
        else:
            handler = logging.FileHandler(self.logfile)
        handler.setFormatter(formatter)
        # attach handler to root logger
        root_logger.addHandler(handler)
        level = logging.INFO
        if self.log_verbose:
            level = logging.DEBUG
        root_logger.setLevel(level)
        log.setLevel(level)
        log.propagate=1
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
            opts, args = getopt.getopt(sys.argv[first_getopt_index:], 'vhp:l:ru:q:', 'help')
        except getopt.GetoptError as err:
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
            elif option == '-u':
                self.starting_url = arg 
            elif option == '-q':
                self.logfile = arg

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

        # this variable is just a place holder and
        # reused whenever urlparse return value has
        # to be used
        
        parse_result = parse.urlparse(self.starting_url)

        # this list holds the ParseResult values
        # which are returned from urlparse
        
        url_list = [ parse_result ]

        # holds hash values of visited urls and maps them to instances
        # of class UrlExtract
        
        visited_urls = {}

        # loop until the whole internet (reachable from starting url) is processed
        
        while url_list:
            try:
                next_parse_result = url_list.pop()
                self.log.debug("Next parse result = {}".format(next_parse_result))
                next_url=None
                
                if self.validate(next_parse_result):
                    next_url = "http://" + next_parse_result.netloc
                    if len(next_parse_result.path) > 1:
                        next_url = next_url + next_parse_result.path
                else:
                    self.log.debug("Validation failed")
                    continue

                md5 = hashlib.md5()
                md5.update(next_url.encode("utf-8"))
                hash_val = md5.hexdigest()
                self.log.debug("Processing {} with hash value {}".format(next_url, hash_val))

                if hash_val not in visited_urls:
                    self.log.info("Visiting {}".format(next_url))
                    extract = UrlExtract(hash_val, next_url)
                    visited_urls[hash_val] = extract
                    lParser = CrawlerHtmlParser()
                    urls = lParser.getLinks(next_url)
                    lParser.close()
                    extract.copy_from_parser(lParser)
                    for url in urls:
                        next_parse_result = parse.urlparse(url)
                        url_list.append(next_parse_result)
                else:
                    self.log.info("Url was already visited")

            except urllib.error.HTTPError as e:
                self.log.info("HTTP-Error code {} received when processing url {}".format(e.code, next_url))
                
            except urllib.error.URLError as e:
                self.log.info("URL-Error {} received when processing url {}".format(e.code, next_url))
                
            except:
                self.log.info("Unexpected error when processing url {}".format(next_url))
                raise
            
        self.log.info("No more urls to process.")

    def validate(self, parse_result):
        if parse_result.scheme != "http":
            return False
        if parse_result.netloc.endswith("aol.com"):
            return False
        if parse_result.netloc.startswith("advertising.aol.com"):
            return False
        if parse_result.netloc.startswith("www.facebook"):
            return False
        if parse_result.netloc.startswith("www.twitter"):
            return False
        if parse_result.netloc.startswith("www.flickr"):
            return False
        if parse_result.netloc.startswith("javascript:"):
            return False
        if parse_result.netloc.endswith("()"):
            return False
        if (len(parse_result.query) > 0):
            return False
        if (len(parse_result.path) > 128):
            return False
        return True

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
