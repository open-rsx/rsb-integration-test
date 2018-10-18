from distutils.spawn import find_executable
import os
import subprocess
import time

import pytest


def pytest_addoption(parser):
    parser.addoption(
        '--languages',
        nargs='+',
        default=['cpp', 'python', 'java'],
        help='Languages to test')
    parser.addoption(
        '--transports',
        nargs='+',
        default=['socket'],
        help='Transports to test')
    parser.addoption(
        '--spread-port',
        type=int,
        default=4802,
        help='Port to use for the spread transport')
    parser.addoption(
        '--socket-port',
        type=int,
        default=55555,
        help='Port to use for the socket transport')
    parser.addoption(
        '--test-timeout',
        type=int,
        default=30,
        help='Timeout for each test in seconds')


def pytest_generate_tests(metafunc):
    for fixture_name in ['language', 'language_one', 'language_two']:
        if fixture_name in metafunc.fixturenames:
            languages = metafunc.config.getoption('languages')
            metafunc.parametrize(fixture_name, languages)

    if 'transport_name' in metafunc.fixturenames:
        transports = metafunc.config.getoption('transports')
        metafunc.parametrize('transport_name', transports)


@pytest.fixture
def transport(transport_name, request):

    class Transport:
        def __init__(self):
            self.name = transport_name
            self.environment = {}

    transport = Transport()

    if transport_name == 'spread':
        spread, socket = '1', '0'
        transport.environment['RSB_PLUGINS_CPP_LOAD'] = 'rsbspread'
        transport.environment['RSB_TRANSPORT_SPREAD_PORT'] = \
            str(request.config.getoption('spread_port'))
    elif transport_name == 'socket':
        spread, socket = '0', '1'
        transport.environment['RSB_TRANSPORT_SOCKET_SERVER'] = 'auto'
        transport.environment['RSB_TRANSPORT_SOCKET_PORT'] = \
            str(request.config.getoption('socket_port'))

    transport.environment['RSB_TRANSPORT_INPROCESS_ENABLED'] = '0'
    transport.environment['RSB_TRANSPORT_SPREAD_ENABLED'] = spread
    transport.environment['RSB_TRANSPORT_SOCKET_ENABLED'] = socket

    return transport


def pytest_runtest_setup(item):
    if 'requires_lisp' in item.keywords \
            and 'lisp' not in item.config.getoption('languages'):
        pytest.skip('requires lisp language')


@pytest.fixture(scope='session', autouse=True)
def configure_spread(request, tmpdir_factory):
    if 'spread' in request.config.getoption('transports'):
        with open("test/spread.conf.in") as template:
            content = template.read().replace(
                '@PORT@',
                str(request.config.getoption('spread_port')))
        config_file = tmpdir_factory.mktemp('spread_config').join('spead.conf')
        config_file.write(content)

        spread_executable = find_executable('spread')
        if not spread_executable:
            pytest.fail('Spread not found. Please put it on PATH')
            return

        os.environ['RSB_TRANSPORT_SPREAD_PORT'] = str(
            request.config.getoption('spread_port'))

        spread_proc = subprocess.Popen([spread_executable, '-n', 'localhost',
                                        '-c', config_file])
        time.sleep(3)
        print('Starting spread')
        yield
        print('Terminating spread')
        spread_proc.terminate()
        spread_proc.wait()
    else:
        yield
