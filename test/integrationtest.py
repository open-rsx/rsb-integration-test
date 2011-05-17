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

from distutils.spawn import find_executable
from optparse import OptionParser

LANG_PYTHON = "python"
LANG_CPP = "cpp"
LANG_JAVA = "java"

binaryExecutorList = {LANG_CPP: [],
                      LANG_JAVA: ["bash"],
                      LANG_PYTHON: []}

binaryPaths = {LANG_CPP: "build/cpp",
               LANG_JAVA: "build/java",
               LANG_PYTHON: "python"}

binaryExtensions = {LANG_CPP: "",
                    LANG_JAVA: ".sh",
                    LANG_PYTHON: ".py"}

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

    def startProcess(self, lang, kind):
        binary = os.path.join(binaryPaths[lang], kind + binaryExtensions[lang])
        commandline = binaryExecutorList[lang] + [binary]

        self.__logger.info("starting %s with command line: %s" % (kind, commandline))

        return subprocess.Popen(commandline)

    @classmethod
    def addPair(clazz, listenerLang, informerLang):
        def testFunc(self):
            # Start listener and informer processes
            listenerProc = self.startProcess(listenerLang, "listener")
            time.sleep(2)
            informerProc = self.startProcess(informerLang, "informer")

            # Wait for both processes to finish.
            waitStart = time.time()
            informerStatus = None
            listenerStatus = None
            while time.time() < waitStart + 20 and (informerStatus == None or listenerStatus == None):
                informerStatus = informerProc.poll()
                listenerStatus = listenerProc.poll()
                time.sleep(0.2)

            self.__logger.info("waiting finished for listener = %s and informer = %s, listenerStauts = %s, informerStatus = %s" % (listenerLang, informerLang, listenerStatus, informerStatus))

            if listenerStatus == None or informerStatus == None:
                # one of the processes timed out
                self.__logger.info("Timeout")
                try:
                    listenerProc.kill()
                except:
                    pass
                try:
                    informerProc.kill()
                except:
                    pass
                self.fail("Timeout receiving messages with a %s listener and a %s informer" % (listenerLang, informerLang))
            else:

                self.assertEqual(0, listenerStatus, "Error of listener, informer language: %s, listener language: %s" % (informerLang, listenerLang))
                self.assertEqual(0, informerStatus, "Error of informer, informer language: %s, listener language: %s" % (informerLang, listenerLang))

                # TODO check message contents parsed from stdout of the listeners
                pass
        setattr(clazz,
                'test' + listenerLang.capitalize() + informerLang.capitalize(),
                testFunc)

def run():
    # Setup logging
    logging.basicConfig()
    logging.getLogger().setLevel(logging.DEBUG)

    # Commandline options
    parser = OptionParser()
    parser.add_option("-s", "--spread", dest="spread", help="spread executable", metavar="executable")
    parser.add_option("-p", "--spread-port",
                      dest    = "port",
                      type    = int,
                      default = 4545,
                      help    = "Number of the port that the Spread daemon should use.")
    (options, args) = parser.parse_args()

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
    os.environ['RSB_TRANSPORT_SPREAD_PORT'] = str(options.port)

    # Add a test method for each pair of languages.
    languages = [LANG_JAVA, LANG_CPP, LANG_PYTHON]
    map(lambda x: IntegrationTest.addPair(*x), itertools.product(languages, languages))

    xmlrunner.XMLTestRunner(output='test-reports').run(unittest.TestLoader().loadTestsFromTestCase(IntegrationTest))

if __name__ == "__main__":
    run()
