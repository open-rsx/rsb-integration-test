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

from distutils.spawn import find_executable

LANG_PYTHON = "python"
LANG_CPP = "cpp"

binaryPaths = {LANG_CPP: "build/cpp",
               LANG_PYTHON: "python"}

binaryExtensions = {LANG_CPP: "",
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

    def testPublishSubscribe(self):

        languages = [LANG_PYTHON, LANG_CPP]

        # evaluate all language combinations
        for informerLang in languages:
            for listenerLang in languages:

                listenerBinary = os.path.join(binaryPaths[listenerLang], "listener" + binaryExtensions[listenerLang])
                listenerCommandLine = [listenerBinary]
                
                self.__logger.info("starting listener with command line: %s" % listenerCommandLine)

                listenerProc = subprocess.Popen(listenerCommandLine)
                
                time.sleep(2)
                
                informerBinary = os.path.join(binaryPaths[informerLang], "informer" + binaryExtensions[informerLang])
                informerCommandLine = [informerBinary]
                
                self.__logger.info("starting informer with command line: %s" % informerCommandLine)
                
                informerProc = subprocess.Popen(informerCommandLine)
                
                # wait for both processes to finish.
                waitStart = time.time()
                informerStatus = None
                listenerStatus = None
                while time.time() < waitStart + 10 and (informerStatus == None or listenerStatus == None):
                    
                    informerStatus = informerProc.poll()
                    listenerStatus = informerProc.poll()
                    
                    time.sleep(0.2)
                    
                self.__logger.info("waiting finished, listenerStauts = %s, informerStatus = %s" % (listenerStatus, informerStatus))
                        
                if listenerStatus == None or informerStatus == None:
                    # one of the processes timed out
                    self.__logger.info("Timeout")
                    listenerProc.kill()
                    informerProc.kill()
                    self.fail("Timeout, listenerStauts = %s, informerStatus = %s" % (listenerStatus, informerStatus))
                else:
                    
                    self.assertEqual(0, listenerStatus, "Error of listener, informer language: %s, listener language: %s" % (informerLang, listenerLang))
                    self.assertEqual(0, informerStatus, "Error of informer, informer language: %s, listener language: %s" % (informerLang, listenerLang))
                    
                    # TODO check message contents parsed from stdout of the listeners
                    pass

def run():

    logging.basicConfig()
    logging.getLogger().setLevel(logging.DEBUG)
    
    spreadExecutable = find_executable("spread")
    spread = None
    if spreadExecutable:
        spread = CommandStarter([spreadExecutable, "-n", "localhost", "-c", "test/spread.conf"])
    
    unittest.main(testRunner=xmlrunner.XMLTestRunner(output='test-reports'))

if __name__ == "__main__":
    run()
