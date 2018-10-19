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

import configparser
import logging
import os
from pathlib import Path
import random
import subprocess
import time

import pytest


test_base = Path(__file__).parent.parent


LANG_PYTHON = 'python'
LANG_CPP = 'cpp'
LANG_JAVA = 'java'
LANG_LISP = 'lisp'

binary_executor_list = {
    LANG_CPP: [],
    LANG_JAVA: [
        'java',
        '-cp',
        os.path.join(
            test_base,
            'build/java/'
            'integration-test-1.0-SNAPSHOT-jar-with-dependencies.jar'),
        '-Djava.net.preferIPv4Stack=true',
        '-Djava.util.logging.config.file=java/logging.properties',
    ],
    LANG_PYTHON: ['python', '-u'],
    LANG_LISP: []
}

environment_files = {
    LANG_CPP: 'cpp/environment',
    LANG_JAVA: None,
    LANG_PYTHON: None,
    LANG_LISP: None
}

binary_paths = {
    LANG_CPP: os.path.join(test_base, 'build/cpp'),
    LANG_JAVA: '',
    LANG_PYTHON: os.path.join(test_base, 'python'),
    LANG_LISP: os.path.join(test_base, 'build/lisp'),
}


def java_name(original):
    new = 'rsb.integration.test.'
    previousSpecial = True
    for s in original:
        if previousSpecial:
            new += s.upper()
            previousSpecial = False
        elif s.isalnum():
            new += s
        else:
            previousSpecial = True
    return new + 'Test'


binary_name_manglers = {
    LANG_CPP: lambda x: x,
    LANG_JAVA: java_name,
    LANG_PYTHON: lambda x: x,
    LANG_LISP: lambda x: x
}

binary_extensions = {
    LANG_CPP: '',
    LANG_JAVA: '',
    LANG_PYTHON: '.py',
    LANG_LISP: ''
}

values = {
    LANG_CPP: {
        'true': '1',
        'false': '0',
        'stringtype': 'std::string'
    },
    LANG_JAVA: {
        'true': 'true',
        'false': 'false',
        'stringtype': 'String'
    },
    LANG_PYTHON: {
        'true': 'True',
        'false': 'False',
        'stringtype': 'str'
    },
    LANG_LISP: {
        'true': '1',
        'false': '0',
        'stringtype': 'string'
    }
}


def start_process(lang, kind, args=[], env=None, output_file=None, cwd=None):
    binary = os.path.join(binary_paths[lang], kind + binary_extensions[lang])
    commandline = binary_executor_list[lang] + \
        [binary_name_manglers[lang](binary)] + list(args)

    print("""starting {}
\twith command line {}
\tand environment   {}""".format(kind, commandline, env))

    environment = dict(os.environ)
    if environment_files[lang]:
        parser = configparser.ConfigParser()
        parser.read([environment_files[lang]])
        lang_env = {k.upper(): v for k, v in parser.items('environment')}
        environment.update(lang_env)
    if env:
        environment.update(env)
    return subprocess.Popen(commandline, env=environment, stderr=output_file,
                            cwd=cwd)


def start_process_and_wait(lang, kind, cwd, timeout=10, **kwargs):
    wait_file = cwd.join('ready')
    print(wait_file)
    proc = start_process(lang, kind, cwd=cwd, **kwargs)
    wait_start = time.time()
    try:
        while not os.path.exists(wait_file):
            if timeout is not None and time.time() > wait_start + timeout:
                kill_processes(proc)
                pytest.fail('Process failed to start correctly')
                return None
            time.sleep(0.1)
        print('{} {} startup took {} seconds'.format(
            lang, kind, time.time() - wait_start))
        return proc
    finally:
        if os.path.exists(wait_file):
            os.remove(wait_file)


def kill_processes(*processes):
    for process in processes:
        try:
            process.kill()
            process.wait()
        except:
            pass


def wait_for_processes(timeout, *processes):
    wait_start = time.time()
    codes = [None] * len(processes)
    while (timeout is None or time.time() < wait_start + timeout) \
            and None in codes:
        codes = [x.poll() for x in processes]
        time.sleep(0.1)
    if None in codes:
        kill_processes(*processes)
    return codes


def analyze_exit_codes(codes, names):
    failed, reason = False, ''
    for code, name in zip(codes, names):
        if code is None:
            print('Timeout; process: {}'.format(name))
            failed = True
            reason += 'timeout of {} process; '.format(name)
        elif not code == 0:
            print('Error; process: {}, exit-code: {}'.format(name, code))
            failed = True
            reason += 'non-zero exit code of {} process; '.format(name)

    assert not failed, reason


def test_event_id(language, tmpdir):
    logfile = tmpdir.join('process.out')
    print('Using logfile {}'.format(logfile))
    process = start_process(
        language, 'event_id',
        output_file=logfile.open('w'))
    assert process.wait() == 0


def test_configuration(language, tmpdir):
    config = 'test/config-smoke.conf'
    output = tmpdir.join('actual')
    expected = 'test/config-smoke.expected'

    logfile = tmpdir.join('process.out')
    print('Using logfile {}'.format(logfile))
    process = start_process(
        language, 'config',
        [config, output],
        output_file=logfile.open('w'))
    process.wait()

    actual = open(output).read()
    expected = open(expected).read()
    for key in ['lang'] + list(values[language].keys()):
        if key == 'lang':
            value = language
        else:
            value = values[language][key]
        expected = expected.replace('@%s@' % key.upper(), value)

    assert actual == expected


def test_pub_sub(transport, language_one, language_two, tmpdir, request):
    listener_logfile = tmpdir.join('listener.out')
    print('Listener log file: {}'.format(listener_logfile))
    informer_logfile = tmpdir.join('informer.out')
    print('Informer log file: {}'.format(informer_logfile))

    listener_proc = start_process_and_wait(
        language_one, 'listener', tmpdir,
        env=transport.environment,
        output_file=listener_logfile.open('w'))

    informer_proc = start_process(
        language_two, 'informer',
        args=['--listener-pid', str(listener_proc.pid)],
        env=transport.environment,
        output_file=informer_logfile.open('w'))

    codes = wait_for_processes(
        request.config.getoption('test_timeout'),
        informer_proc, listener_proc)
    analyze_exit_codes(codes, ('informer', 'listener'))


def test_rpc(transport, language_one, language_two, tmpdir, request):
    server_logfile = tmpdir.join('server.out')
    print('Server log file: {}'.format(server_logfile))
    client_logfile = tmpdir.join('client.out')
    print('Client log file: {}'.format(client_logfile))

    cookie = str(random.randint(0, 1 << 31))

    server_proc = start_process_and_wait(
        language_one, 'server', tmpdir,
        args=['--cookie', cookie],
        env=transport.environment,
        output_file=server_logfile.open('w'))

    client_proc = start_process(
        language_two, 'client',
        args=['--cookie', cookie],
        env=transport.environment,
        output_file=client_logfile.open('w'))

    codes = wait_for_processes(
        request.config.getoption('test_timeout'),
        client_proc, server_proc)
    analyze_exit_codes(codes, ('client', 'server'))


@pytest.mark.requires_lisp
def test_introspection(transport, language, tmpdir, request):
    local_logfile = tmpdir.join('local.out')
    print('Local log file: {}'.format(local_logfile))
    remote_logfile = tmpdir.join('remote.out')
    print('Remote log file: {}'.format(remote_logfile))

    cookie = str(random.randint(0, 1 << 31))

    remote_proc = start_process_and_wait(
        'lisp', 'remote-introspection', tmpdir,
        args=['--cookie', cookie],
        env=transport.environment,
        output_file=remote_logfile.open('w'))

    env = dict(transport.environment)
    env['RSB_PLUGINS_CPP_LOAD'] = ':'.join(
        env.get('RSB_PLUGINS_CPP_LOAD', '').split(':') + ['rsbintrospection'])
    env['RSB_INTROSPECTION_ENABLED'] = '1'

    local_proc = start_process(
        language, 'local-introspection',
        args=['--cookie', cookie],
        env=env,
        output_file=local_logfile.open('w'))

    codes = wait_for_processes(
        request.config.getoption('test_timeout'),
        local_proc, remote_proc)
    analyze_exit_codes(codes, ('local', 'remote'))
