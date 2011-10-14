# ============================================================
#
# Copyright (C) 2011 by Johannes Wienke <jwienke at techfak dot uni-bielefeld dot de>
#
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation;
# either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# ============================================================

import unittest
import logging
import xmlrunner
import subprocess
import os
import time
import itertools
import random

from distutils.spawn import find_executable
from optparse import OptionParser

LANG_PYTHON = "python"
LANG_CPP = "cpp"
LANG_JAVA = "java"
LANG_LISP = "lisp"

languages = [ LANG_PYTHON, LANG_CPP, LANG_JAVA, LANG_LISP ]

binaryExecutorList = {LANG_CPP: [],
                      LANG_JAVA: ["bash"],
                      LANG_PYTHON: [],
                      LANG_LISP: []}

binaryPaths = {LANG_CPP: "build/cpp",
               LANG_JAVA: "build/java",
               LANG_PYTHON: "python",
               LANG_LISP: "build/lisp"}

binaryExtensions = {LANG_CPP: "",
                    LANG_JAVA: ".sh",
                    LANG_PYTHON: ".py",
                    LANG_LISP: ""}

tests = [ "parser", "id", "pubsub", "rpc" ]

values= {LANG_CPP:    {'true':       '1',
                       'false':      '0',
                       'stringtype': 'std::string'},
         LANG_JAVA:   {'true':       'true',
                       'false':      'false',
                       'stringtype': 'String'},
         LANG_PYTHON: {'true':       'True',
                       'false':      'False',
                       'stringtype': 'str'},
         LANG_LISP:   {'true':       '1',
                       'false':      '0',
                       'stringtype': 'string'} }

class CommandStarter(object):
    """
    Starts a command and terminates it on destruction.

    @author: jwienke
    """

    def __init__(self, command):
        self.__open = subprocess.Popen(command)
        time.sleep(2)

    def __del__(self):
        print("Stopping command %s" % self.__open)
        self.__open.terminate()
        self.__open.wait()

class IntegrationTest(unittest.TestCase):
    """
    Python test runner to execute the unit tests.

    @author: jwienke
    """

    def setUp(self):
        self.__logger = logging.getLogger("IntegrationTest")

    def tearDown(self):
        pass

    def startProcess(self, lang, kind, *args):
        binary = os.path.join(binaryPaths[lang], kind + binaryExtensions[lang])
        commandline = binaryExecutorList[lang] + [binary] + list(args)

        self.__logger.info("starting %s with command line: %s" % (kind, commandline))

        return subprocess.Popen(commandline)

    def waitForProcesses(self, timeout, *processes):
        waitStart = time.time()
        codes     = [None]*len(processes)
        while time.time() < waitStart + timeout and None in codes:
            codes = map(lambda x: x.poll(), processes)
            time.sleep(0.2)
        return codes

    def killProcesses(self, *processes):
        for process in processes:
            try:
                process.kill()
            except:
                pass

    @classmethod
    def addListenerInformerPair(clazz, listenerLang, informerLang):
        def testFunc(self):
            # Start listener and informer processes
            waitFile = 'test/%s-listener-ready' % listenerLang
            if os.path.exists(waitFile):
                self.__logger.warn("Deleting old waitFile %s" % waitFile)
                os.remove(waitFile)
            listenerProc = self.startProcess(listenerLang, "listener")
            waitStart = time.time()
            while not os.path.exists(waitFile):
                if time.time() > waitStart + 5:
                    self.killProcesses(listenerProc)
                    self.fail("Timeout while waiting for %s listener to start" % listenerLang)
                time.sleep(0.2)
            os.remove(waitFile)
            time.sleep(1)

            self.__logger.info("%s listener startup took %s seconds"
                               % (listenerLang, time.time() - waitStart))
            informerProc = self.startProcess(informerLang, "informer",
                                             "--listener-pid", str(listenerProc.pid))
            time.sleep(1)

            informerStatus, listenerStatus = self.waitForProcesses(60, informerProc, listenerProc)

            self.__logger.info("waiting finished for listener = %s and informer = %s, listenerStauts = %s, informerStatus = %s" % (listenerLang, informerLang, listenerStatus, informerStatus))

            if listenerStatus == None or informerStatus == None:
                # one of the processes timed out
                self.__logger.info("Timeout")
                self.killProcesses(listenerProc, informerProc)
                self.fail("Timeout receiving messages with a %s listener and a %s informer" % (listenerLang, informerLang))

            self.assertEqual(0, listenerStatus, "Error of listener, informer language: %s, listener language: %s" % (informerLang, listenerLang))
            self.assertEqual(0, informerStatus, "Error of informer, informer language: %s, listener language: %s" % (informerLang, listenerLang))

            # TODO check message contents parsed from stdout of the listeners
        setattr(clazz,
                'testListenerInformer' + listenerLang.capitalize() + informerLang.capitalize(),
                testFunc)

    @classmethod
    def addClientServerPair(clazz, clientLang, serverLang):
        def testFunc(self):
            cookie = str(random.randint(0, 1 << 31))
            serverProc = self.startProcess(serverLang, "server",
                                           "--cookie", cookie)
            time.sleep(1) # TODO proper waiting
            clientProc = self.startProcess(clientLang, "client",
                                           "--cookie", cookie)

            codes = self.waitForProcesses(30, clientProc, serverProc)
            if None in codes:
                self.__logger.info("Timeout")
                self.killProcesses(serverProc, clientProc)
                self.fail("Client/Server communication timed out for %s client and %s server"
                          % (clientLang, serverLang))

        setattr(clazz,
                'testClientServer' + clientLang.capitalize() + serverLang.capitalize(),
                testFunc)

    @classmethod
    def addParserTest(clazz, lang):
        def testFunc(self):
            input    = 'test/config-smoke.conf'
            output   = 'test/config-smoke-%s.output' % lang
            expected = 'test/config-smoke.expected'

            configProc = self.startProcess(lang, 'config', input, output)
            configProc.wait()

            actual = open(output).read()
            expected = open(expected).read()
            for key in ['lang'] + values[lang].keys():
                if key == 'lang':
                    value = lang
                else:
                    value = values[lang][key]
                expected = expected.replace('@%s@' % key.upper(), value)
            print expected

            self.assertEqual(actual, expected)
        setattr(clazz,
                'testConfiguration' + lang.capitalize(),
                testFunc)

    @classmethod
    def addEventIdTest(clazz, lang):
        def testFunc(self):
            configProc = self.startProcess(lang, 'event_id')
            if configProc.wait() != 0:
                self.fail('Event id generation test return non-zero exit code')
        setattr(clazz,
                'testEventIdGeneration' + lang.capitalize(),
                testFunc)


def run():
    # Setup logging
    logging.basicConfig()
    logging.getLogger().setLevel(logging.DEBUG)

    # Commandline options
    parser = OptionParser(usage = ("usage: %%prog [OPTIONS] [LANG1 [LANG2 [...]]]\nwhere LANGN in %s"
                                   % languages))
    parser.add_option("-s", "--spread",
                      dest    = "spread",
                      metavar = "EXECUTABLE",
                      help    = "spread executable")
    parser.add_option("-p", "--spread-port",
                      dest    = "port",
                      type    = int,
                      default = 4545,
                      help    = "Number of the port that the Spread daemon should use.")
    parser.add_option("-t", "--test",
                      dest    = "tests",
                      action  = "append",
                      metavar = "CATEGORY",
                      help    = ("Test categories that should be executed. Can be supplied multiple times. Valid categories are: %s"
                                 % tests))
    (options, args) = parser.parse_args()

    # Restrict languages to specified ones.
    selectedLanguages = languages
    if len(args) > 0:
        selectedLanguages = args

    # Determine tests categories to run.
    if options.tests:
        selectedTests = options.tests
    else:
        selectedTests = tests

    # Prepare config file and launch spread
    with open("test/spread.conf.in") as template:
        content = template.read().replace('@PORT@', str(options.port))
    with open("test/spread.conf", "w") as config:
        config.write(content)

    spreadExecutable = find_executable("spread")
    if options.spread:
        spreadExecutable = options.spread
    spread = None
    if spreadExecutable:
        spread = CommandStarter([spreadExecutable, "-n", "localhost", "-c", "test/spread.conf"])

    # Export configured spread port into configuration variable
    os.environ['RSB_TRANSPORT_INPROCESS_ENABLED'] = '0'
    os.environ['RSB_TRANSPORT_SPREAD_ENABLED']    = '1'
    os.environ['RSB_TRANSPORT_SPREAD_PORT']       = str(options.port)

    # Add a test method for the configuration test for each language.
    if "parser" in selectedTests:
        map(IntegrationTest.addParserTest, selectedLanguages)

    # Add a test method for the event id generation mechanism for each
    # language.
    if "id" in selectedTests:
        map(IntegrationTest.addEventIdTest, selectedLanguages)

    # Add a test method for the listener/informer communication test
    # for each pair of languages.
    if "pubsub" in selectedTests:
        map(lambda x: IntegrationTest.addListenerInformerPair(*x),
            itertools.product(selectedLanguages, selectedLanguages))

    # Add a test method for the client/server communication test for
    # each pair of languages.
    if "rpc" in selectedTests:
        map(lambda x: IntegrationTest.addClientServerPair(*x),
            itertools.product(selectedLanguages, selectedLanguages))

    # Execute the generated test suite.
    xmlrunner.XMLTestRunner(output='test-reports').run(unittest.TestLoader().loadTestsFromTestCase(IntegrationTest))

if __name__ == "__main__":
    run()
