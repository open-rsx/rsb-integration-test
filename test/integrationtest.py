# ============================================================
#
# Copyright (C) 2011 by Johannes Wienke <jwienke at techfak dot uni-bielefeld dot de>
# Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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
import ConfigParser

from distutils.spawn import find_executable
from optparse import OptionParser

LANG_PYTHON = "python"
LANG_CPP    = "cpp"
LANG_JAVA   = "java"
LANG_LISP   = "lisp"

languages = [ LANG_PYTHON, LANG_CPP, LANG_JAVA, LANG_LISP ]

binaryExecutorList = { LANG_CPP:    [],
                       LANG_JAVA:   [ "java", "-Djava.net.preferIPv4Stack=true" ],
                       LANG_PYTHON: [],
                       LANG_LISP:   []}

environmentFiles = { LANG_CPP:    None,
                     LANG_JAVA:   "build/java/environment",
                     LANG_PYTHON: None,
                     LANG_LISP:   None}

binaryPaths = { LANG_CPP:    "build/cpp",
                LANG_JAVA:   "",
                LANG_PYTHON: "python",
                LANG_LISP:   "build/lisp" }

def javaName(original):
    new = ""
    previousSpecial = True
    for s in original:
        if previousSpecial:
            new += s.upper()
            previousSpecial = False
        elif s.isalnum():
            new += s
        else:
            previousSpecial = True
    return new + "Test"

binaryNameManglers = { LANG_CPP:    lambda x: x,
                       LANG_JAVA:   javaName,
                       LANG_PYTHON: lambda x: x,
                       LANG_LISP:   lambda x: x }

binaryExtensions = { LANG_CPP:    "",
                     LANG_JAVA:   "",
                     LANG_PYTHON: ".py",
                     LANG_LISP:   "" }

transports = [ "spread", "socket" ]

tests = [ 'parser', 'id', 'pubsub', 'rpc', 'introspection' ]

values= { LANG_CPP:    { 'true':       '1',
                         'false':      '0',
                         'stringtype': 'std::string' },
          LANG_JAVA:   { 'true':       'true',
                         'false':      'false',
                         'stringtype': 'String' },
          LANG_PYTHON: { 'true':       'True',
                         'false':      'False',
                         'stringtype': 'str' },
          LANG_LISP:   { 'true':       '1',
                         'false':      '0',
                         'stringtype': 'string' } }

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

    def startProcess(self, lang, kind, args = [], env = None):
        binary = os.path.join(binaryPaths[lang], kind + binaryExtensions[lang])
        commandline = binaryExecutorList[lang] + [binaryNameManglers[lang](binary)] + list(args)

        self.__logger.info("""starting %s
\twith command line %s
\tand environment   %s"""
                           % (kind, commandline, env))

        environment = dict(os.environ)
        if environmentFiles[lang]:
            parser = ConfigParser.ConfigParser()
            parser.read([environmentFiles[lang]])
            langEnv = {k.upper(): v for k,v in parser.items('environment')}
            environment.update(langEnv)
        if env:
            environment.update(env)
        return subprocess.Popen(commandline, env = environment)

    def killProcesses(self, *processes):
        for process in processes:
            try:
                process.kill()
            except:
                pass

    def waitForProcesses(self, timeout, *processes):
        waitStart = time.time()
        codes     = [None]*len(processes)
        timeout   = self.getTimeout(timeout)
        while (timeout is None or time.time() < waitStart + timeout) \
              and None in codes:
            codes = map(lambda x: x.poll(), processes)
            time.sleep(0.1)
        if None in codes:
            self.killProcesses(*processes)
        return codes

    def startProcessAndWait(self, lang, kind, args = [], env = None, timeout = 5):
        waitFile = 'test/%s-%s-ready' % (lang, kind)
        if os.path.exists(waitFile):
            self.__logger.warn("Deleting old waitFile %s" % waitFile)
            os.remove(waitFile)
        proc = self.startProcess(lang, kind, args = args, env = env)
        waitStart = time.time()
        timeout = self.getTimeout(timeout)
        try:
            while not os.path.exists(waitFile):
                if not timeout is None \
                   and time.time() > waitStart + timeout:
                    self.killProcesses(proc)
                    return None
                time.sleep(0.1)
            self.__logger.info("%s %s startup took %s seconds"
                               % (lang, kind, time.time() - waitStart))
            return proc
        finally:
            if os.path.exists(waitFile):
                os.remove(waitFile)

    def analyzeExitCodes(self, codes, names):
        failed, reason = False, ""
        for code, name in zip(codes, names):
            if code is None:
                self.__logger.info("Timeout; process: %s", name)
                failed = True
                reason += 'timeout of %s process; ' % name
            elif not code == 0:
                self.__logger.info("Error; process: %s, exit-code: %s", name, code)
                failed = True
                reason += 'non-zero exit code of %s process; ' % name
        return failed, reason

    timeout = 1

    nextSocketPort = 0

    @classmethod
    def getTimeout(clazz, factor):
        if not clazz.timeout is None:
            return clazz.timeout * factor

    @classmethod
    def prepareTransportConfiguration(clazz, transport):
        if transport == 'spread':
            spread, socket = '1', '0'
            options1 = { 'RSB_PLUGINS_CPP_LOAD': 'rsbspread' }
            options2 = { 'RSB_PLUGINS_CPP_LOAD': 'rsbspread' }
        elif transport == 'socket':
            spread, socket = '0', '1'
            options1 = { 'RSB_TRANSPORT_SOCKET_PORT': str(clazz.nextSocketPort) }
            options2 = { 'RSB_TRANSPORT_SOCKET_PORT': str(clazz.nextSocketPort) }
            clazz.nextSocketPort += 1
        else:
            raise ValueError, "Unknown transport `%s'" % transport

        os.environ['RSB_TRANSPORT_INPROCESS_ENABLED'] = '0'
        os.environ['RSB_TRANSPORT_SPREAD_ENABLED']    = spread
        os.environ['RSB_TRANSPORT_SOCKET_ENABLED']    = socket

        return options1, options2

    @classmethod
    def addTestPair(clazz, kind, transport, info1, info2, execute, timeout = 30):
        (role1, language1) = info1
        (role2, language2) = info2
        def notDash(char):
            return not char == '-'
        methodName = 'test%s%s%s%s%s' \
                     % (filter(notDash, role1.capitalize()),
                        filter(notDash, role2.capitalize()),
                        transport.capitalize(),
                        language1.capitalize(), language2.capitalize())

        def testFunc(self):
            self.__logger.info('\n\n\n==================== KIND: %s - TRANSPORT: %s - %s: %s - %s: %s ====================' %
                               (kind, transport,
                                role1.upper(), language1,
                                role2.upper(), language2))

            # Prepare environment
            options1, options2 = clazz.prepareTransportConfiguration(transport)
            process1, process2 = execute(self, options1, options2)

            codes = self.waitForProcesses(clazz.getTimeout(timeout), process1, process2)
            failed, reason = self.analyzeExitCodes(codes, (role1, role2))
            if failed:
                self.fail("%s/%s communication failed for %s %s and %s %s: %s"
                          % (role1, role2,
                             language1, role1, language2, role2, reason))

        setattr(clazz, methodName, testFunc)

    @classmethod
    def addListenerInformerPair(clazz, transport, listenerLang, informerLang):
        def execute(self, options1, options2):
            # Start listener and informer processes
            listenerProc = self.startProcessAndWait(listenerLang, "listener",
                                                    env = options1)
            if not listenerProc:
                self.fail("Timeout while waiting for %s listener to start"
                          % listenerLang)
            informerProc = self.startProcess(informerLang, "informer",
                                             [ "--listener-pid", str(listenerProc.pid) ],
                                             env = options2)
            return listenerProc, informerProc

        # TODO check message contents parsed from stdout of the listeners

        clazz.addTestPair('pubsub', transport,
                          ('listener', listenerLang),
                          ('informer', informerLang),
                          execute)

    @classmethod
    def addClientServerPair(clazz, transport, clientLang, serverLang):
        def execute(self, options1, options2):
            # Start client and server processes
            cookie = str(random.randint(0, 1 << 31))
            serverProc = self.startProcessAndWait(serverLang, "server",
                                                  [ "--cookie", cookie ],
                                                  env = options2)
            if not serverProc:
                self.fail("Timeout while waiting for %s server to start"
                          % serverLang)
            clientProc = self.startProcess(clientLang, "client",
                                           [ "--cookie", cookie ],
                                           env = options1)
            return clientProc, serverProc

        clazz.addTestPair('rpc', transport,
                          ('client', clientLang),
                          ('server', serverLang),
                          execute)

    @classmethod
    def addIntrospectionPair(clazz, transport, localLang, remoteLang):
        if not remoteLang == 'lisp':
            return

        def execute(self, options1, options2):
            # Start local and remote introspection processes
            cookie = str(random.randint(0, 1 << 31))
            remoteProc = self.startProcessAndWait(remoteLang, "remote-introspection",
                                                  [ "--cookie", cookie ],
                                                  env = options2)
            if not remoteProc:
                self.fail("Timeout while waiting for %s remote-introspection to start"
                          % remoteLang)
            options1['RSB_PLUGINS_CPP_LOAD'] \
                = ':'.join(options1.get('RSB_PLUGINS_CPP_LOAD', '').split(':')
                           + [ 'rsbintrospection' ])
            options1['RSB_INTROSPECTION_ENABLED'] = '1'
            localProc = self.startProcess(localLang, 'local-introspection',
                                          [ "--cookie", cookie ],
                                          env = options1)
            return localProc, remoteProc

        clazz.addTestPair('introspection', transport,
                          ('local-introspection',  localLang),
                          ('remote-introspection', remoteLang),
                          execute, timeout = 20)

    @classmethod
    def addParserTest(clazz, lang):
        def testFunc(self):
            input    = 'test/config-smoke.conf'
            output   = 'test/config-smoke-%s.output' % lang
            expected = 'test/config-smoke.expected'

            configProc = self.startProcess(lang, 'config', [ input, output ])
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

    # General options
    parser.add_option("-t", "--test",
                      dest    = "tests",
                      action  = "append",
                      metavar = "CATEGORY",
                      help    = ("Test categories that should be executed. Can be supplied multiple times. Valid categories are: %s"
                                 % tests))
    parser.add_option("-r", "--transport",
                      dest    = "transports",
                      action  = "append",
                      metavar = "TRANSPORT",
                      help    = ("Transports for which tests should be executed. Can be supplied multiple times. Valid transports are: %s"
                                 % transports))
    parser.add_option("-n", "--no-timeout",
                      dest    = "noTimeout",
                      action = 'store_true',
                      help    = "Disable all timeouts")

    # Spread options
    parser.add_option("-s", "--spread",
                      dest    = "spreadExecutable",
                      metavar = "EXECUTABLE",
                      help    = "spread executable")
    parser.add_option("-p", "--spread-port",
                      dest    = "spreadPort",
                      type    = int,
                      default = 4545,
                      help    = "Number of the port that the Spread daemon should use.")

    # Socket options
    parser.add_option("-o", "--socket-port",
                      dest    = "socketPort",
                      type    = int,
                      default = 7777,
                      help    = "Number of the port that the socket transport should use.")

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

    # Determine transports to use.
    if options.transports:
        selectedTransports = options.transports
    else:
        selectedTransports = transports

    # Prepare config file and launch spread
    if 'spread' in selectedTransports:
        with open("test/spread.conf.in") as template:
            content = template.read().replace('@PORT@', str(options.spreadPort))
        with open("test/spread.conf", "w") as config:
            config.write(content)

        spreadExecutable = find_executable("spread")
        if options.spreadExecutable:
            spreadExecutable = options.spreadExecutable
        spread = None
        if spreadExecutable:
            spread = CommandStarter([spreadExecutable, "-n", "localhost", "-c", "test/spread.conf"])

    # Export configured spread port into configuration variable
    if 'spread' in selectedTransports:
        os.environ['RSB_TRANSPORT_SPREAD_PORT'] = str(options.spreadPort)

    if 'socket' in selectedTransports:
        IntegrationTest.nextSocketPort = options.socketPort
        os.environ['RSB_TRANSPORT_SOCKET_SERVER'] = 'auto'

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
            itertools.product(selectedTransports, selectedLanguages, selectedLanguages))

    # Add a test method for the client/server communication test for
    # each pair of languages.
    if "rpc" in selectedTests:
        map(lambda x: IntegrationTest.addClientServerPair(*x),
            itertools.product(selectedTransports, selectedLanguages, selectedLanguages))

    if 'introspection' in selectedTests:
        map(lambda x: IntegrationTest.addIntrospectionPair(*x),
            itertools.product(selectedTransports, selectedLanguages, selectedLanguages))

    if options.noTimeout:
        IntegrationTest.timeout = None

    # Execute the generated test suite.
    xmlrunner.XMLTestRunner(output='test-reports').run(unittest.TestLoader().loadTestsFromTestCase(IntegrationTest))

if __name__ == "__main__":
    run()
