/* ============================================================
 *
 * This file is a part of the RSB project
 *
 * Copyright (C) 2011, 2012, 2013 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
 *
 * This program is free software; you can redistribute it
 * and/or modify it under the terms of the GNU General
 * Public License as published by the Free Software Foundation;
 * either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * ============================================================ */

#include <stdlib.h>

#include <iostream>
#include <fstream>

#include <rsc/config/ConfigFileSource.h>

#include <rsb/ParticipantConfig.h>

using namespace std;
using namespace rsb;

int main(int argc, char* argv[]) {
    if (argc != 3) {
        return EXIT_FAILURE;
    }

    ParticipantConfig config;
    {
        ifstream stream(argv[1]);
        rsc::config::ConfigFileSource source (stream);
        source.provideOptions(config);
    }
    ofstream out(argv[2]);
    out << "qualityofservice.reliability: ";
    switch (config.getQualityOfServiceSpec().getReliability()) {
    case QualityOfServiceSpec::RELIABLE:
        out << "RELIABLE";
        break;
    case QualityOfServiceSpec::UNRELIABLE:
        out << "UNRELIABLE";
        break;
    }
    out << endl;
    out << "qualityofservice.ordering: ";
    switch (config.getQualityOfServiceSpec().getOrdering()) {
    case QualityOfServiceSpec::ORDERED:
        out << "ORDERED";
        break;
    case QualityOfServiceSpec::UNORDERED:
        out << "UNORDERED";
        break;
    }
    out << endl;
    out << "errorhandling.onhandlererror: ";
    switch (config.getErrorStrategy()) {
    case ParticipantConfig::ERROR_STRATEGY_LOG:
        out << "LOG";
        break;
    case ParticipantConfig::ERROR_STRATEGY_PRINT:
        out << "PRINT";
        break;
    case ParticipantConfig::ERROR_STRATEGY_EXIT:
        out << "EXIT";
        break;
    }
    out << endl;

    ParticipantConfig::Transport spread = config.getTransport("spread");
    out << "transport.inprocess.enabled: " << '0' << endl;
    out << "transport.spread.host: " << spread.getOptions().get < string
                                                                  > ("host") << endl;
    out << "transport.spread.port: " << spread.getOptions().get < string
                                                                  > ("port") << endl;
    out << "transport.spread.enabled: " << spread.isEnabled() << endl;

    ParticipantConfig::Transport::ConverterNames names = spread.getConverters();

    for (ParticipantConfig::Transport::ConverterNames::const_iterator it =
             names.begin(); it != names.end(); ++it) {
        if (it->first == "utf-8-string") {
            out << "transport.spread.converter.cpp.utf-8-string: " << it->second
                << endl;
        }
    }

    return EXIT_SUCCESS;
}
