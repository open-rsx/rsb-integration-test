=====================
 RSB Integrationtest
=====================

Building the Tests
==================

TODO

Running Tests
=============

Integrationtests are executed by running variants of the following
command from the project root directory:

.. code-block:: sh

   $ pytest

When invoked without commandline options like in the above example,
the test runner will execute all tests for all implementations and all
available transports.

When invoked with the ``--help`` commandline option, the test runner
prints a help text which explains how to run tests selectively.

Project Architecture
====================

This project consists of a test runner and test programs written in
the RSB implementation languages, yielding the following directory and
file organization::

  ├── CMakeLists.txt
  ├── test                                   Test runner infrastructure
  │   ├── config-smoke.conf
  │   ├── config-smoke.expected
  │   ├── integrationtest.py                 Test runner
  │   └── spread.conf.in                     Configuration template for Spread tests
  ├── data
  │   ├── generate-event-id-test-cases.lisp
  │   ├── event-id-cases.txt
  │   ├── Image.proto
  │   └── README.txt
  ├── cpp                                    C++ implementation
  │   ├── CMakeLists.txt
  │   ├── event_id.cpp
  │   ├── config.cpp
  │   ├── listener.cpp
  │   ├── informer.cpp
  │   ├── client.cpp
  │   ├── server.cpp
  │   └── local-introspection.cpp
  ├── java                                   Java implementation
  │   ├── CMakeLists.txt
  │   ├── Client.java
  │   ├── Config.java
  │   ├── EventId.java
  │   ├── Informer.java
  │   ├── Listener.java
  │   ├── Server.java
  │   └── LocalIntrospection.java
  ├── lisp                                   Common Lisp implementation
  │   ├── CMakeLists.txt
  │   ├── event_id.lisp
  │   ├── config.lisp
  │   ├── listener.lisp
  │   ├── informer.lisp
  │   ├── client.lisp
  │   ├── server.lisp
  │   ├── local-introspection.lisp
  │   └── remote-introspection.lisp
  └── python                                 Python implementation
      ├── CMakeLists.txt
      ├── config.py
      ├── event_id.py
      ├── listener.py
      ├── informer.py
      ├── client.py
      ├── server.py
      └── local-introspection.py

Tests
=====

Configuration Test
------------------

TODO

ID Test
-------

TODO

Publish/Subscribe Test
----------------------

TODO

Client/Server Test
------------------

TODO

Introspection Test
------------------

This test ensures that implementations generate correct introspection
events and that implementations can receive and interpret these
events.

Estimation of clock-offsets and latencies based on introspection
communication is not tested since the estimation process takes too
much time and expected results hard to specify.

The test works as follows:

#. Initialization of the ``remote-introspection`` process

   #. The runner starts the ``remote-introspection`` program passing
      it a random "cookie" as a commandline option. The configuration
      of this program includes the equivalent of
      ``introspection.enabled = false`` to prevent its participants
      from sending introspection events.

   #. The ``remote-introspection`` process creates a local-server on
      scope ``/rsb-integration-test/introspection`` with a method
      "remote-start" that accepts an integer argument and a method
      "remote-step" that does not accept any arguments.

   #. The ``remote-introspection`` process creates a
      remote-introspection participant and is thus prepared to receive
      introspection events.

      Note that the initial introspection survey performed by this
      participant does not produce any replies since the
      local-introspection process is not running at this point.

   #. The ``remote-introspection`` process writes a file
      :samp:`test/{LANGUAGE}-introspection-ready` which is detected by
      the test runner.

   #. The process then waits until the "remote-start" method is
      called.

#. Initialization of the ``local-introspection`` process

   #. The test runner starts the ``local-introspection`` program,
      passing it the same random "cookie" as a commandline option.

      TODO configuration?

      The value of the "cookie" is not used directly but serves as a
      commandline option which the ``remote-introspection`` process
      knows (since it received the same commandline option from the
      test runner) and can thus verify.

   #. The ``local-introspection`` process creates remote-server
      participant on the scope ``/rsb-integration-test/introspection``
      for which no introspection events are sent.

   #. The process creates a local-server participant on the scope
      ``/rsb-integration-test/introspection`` with a "local-step"
      method that does not accept any arguments.

   #. The process calls the "remote-start" method with its PID as the
      argument.

   #. The process then waits until the "local-step" method is called.

#. Verification of introspection data

   #. The ``remote-introspection`` process detects that the
      "remote-start" method has been called and counts the received
      introspection events caused by the startup of the
      ``local-introspection`` process.

   #. After receiving the expected number of events, the
      ``remote-introspection`` process creates a "first snapshot"" of
      the introspection database.

      This snapshot is compared against an expected state. The
      expected state consists of an entry for the host, an entry for
      the ``local-introspection`` process, and entries for the
      local-server and local-method participants in that process.

      A test failure is recorded if the two do not match.

   #. The ``remote-introspection`` process creates another
      remote-introspection participant which surveys the system,
      thereby populating its database.

      TODO drain events

      The resulting snapshot is compared against the same expectation
      as before. Obviously, both snapshot should be identical.

TODO

#. The ``remote-introspection`` process call
