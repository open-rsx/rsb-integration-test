#include <fstream>
#include <iomanip>

#include <rsc/runtime/TypeStringTools.h>

#include <rsb/Event.h>

using namespace std;

using namespace boost;

using namespace rsc::misc;
using namespace rsc::runtime;

using namespace rsb;

int main() {
	ifstream stream("data/event-id-cases.txt");
	while (!stream.eof()) {
		string origin, expected;
		uint32_t sequenceNumber;
		stream >> origin >> hex >> sequenceNumber >> expected;

		UUID originId(origin), expectedId(expected);
		Event e(Scope("/"), shared_ptr<string>(new string("")),
				typeName<string>());
		e.setEventId(originId, sequenceNumber);
		cout << originId << " " << hex << setw(8) << setfill('0')
				<< sequenceNumber << " => " << e.getId() << " [" << expectedId
				<< "]" << endl;
		assert(expectedId == e.getId());
	}

	return EXIT_SUCCESS;
}
