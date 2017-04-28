#include <bits/stdc++.h>
using namespace std;

// Checks if target is an ER or not
bool check(string target, set <string> entities) {
	bool isitrefexp = true;
	for (set <string>::iterator it = entities.begin() ; it != entities.end() ; it++) {
		if (target.find(*it) != -1) {
			isitrefexp = false;
		}
	}
	return isitrefexp;
}

int main() {

	string currentline = "";
	string currentexp = "";
	string targetstr = "";
	// The following are the targets of the experiment
	string target1 = "rest3", target2 = "rest2";
	int cases = 0;
	set <string> entities;
	string first_er = "", second_er = "";
	multiset <string> results;
	map <string, int> resultsre;
	bool erplural = false;

	while (getline(cin, currentline)) {
		//cout << currentline << endl;
		if (currentline.find("--") != -1) {
			cases++;
			entities.clear();
		} else if (currentline.find("T:") != -1) {
			first_er = "";
			second_er = "";
			erplural = false;
			entities.clear();
			for (int i = 0; i < currentline.size() ; ++i) {
				if (currentline[i] == ':' || currentline[i] == ',') {
					currentline[i] = ' ';
				}
			}
			istringstream iss;
		    iss.str(currentline);
		    iss.clear();
		    string aux = "";
		    while (iss >> aux) {
		    	if (aux != "d" && aux != "c") {
		    		entities.insert(aux);
		    	}
		    }

		    //cout << "Las entidades en el caso ";
		    //cout << cases;
		    //cout << " son: " << endl;
	    	//for (set <string>::iterator it = entities.begin() ; it != entities.end() ; it++) {
			//	cout << *it << endl;
			//}
		} else if (currentline.find(":") != -1) {
 			size_t pos = currentline.find(":");
 			currentexp = currentline.substr(0,pos);
			targetstr = currentline.substr(pos + 1,currentline.size()-1);

			// Here is the check to see if the string is an ER.
			if (targetstr.find(target1) != -1 && targetstr.find(target2) != -1 && !(erplural)) {
				// The idea is to remove the targets from the string
				size_t tar1pos = targetstr.find(target1);
				targetstr = targetstr.replace(tar1pos,target1.size(),"");
				size_t tar2pos = targetstr.find(target2);
				targetstr = targetstr.replace(tar2pos,target2.size(),"");
				//cout << targetstr << endl;
				//cout << "y tenemos que la parte anterior es: " << currentline << endl;
				if (check(targetstr, entities)) {
					results.insert(currentexp);
					map <string, int>::iterator it = resultsre.find(currentexp);
					if (it != resultsre.end()) {
						resultsre[currentexp] = resultsre[currentexp] + 1;
					} else {
						resultsre[currentexp] = 1;
					}
					erplural = true;
				}
			} else if (targetstr.find(target1) != -1 && targetstr.find(target2) == -1) {
				size_t tar1pos = targetstr.find(target1);
				targetstr = targetstr.replace(tar1pos,target1.size(),"");
				if (check(targetstr,entities)) {
					if (first_er != "") {
						if (first_er.size() > currentexp.size()) {
							first_er = currentexp;
						}
					} else first_er = currentexp;
				}

			} else if (targetstr.find(target1) == -1 && targetstr.find(target2) != -1) {
				size_t tar2pos = targetstr.find(target2);
				targetstr = targetstr.replace(tar2pos,target2.size(),"");
				if (check(targetstr,entities)) {
					if (second_er != "") {
						if (second_er.size() > currentexp.size()) {
							second_er = currentexp;
						}
					} else second_er = currentexp;
				}
			}
			if (second_er != "" && first_er != "" && !(erplural)) {
				erplural = true;
				string pluraler = first_er + " && " + second_er;
				string pluraler2 = second_er + " && " + first_er;
				// Este if es para evitar repeticiones
				if (results.find(pluraler) == results.end() && results.find(pluraler2) == results.end()) {
					results.insert(pluraler);
					map <string, int>::iterator it = resultsre.find(pluraler);
					if (it != resultsre.end()) {
						resultsre[pluraler] = resultsre[pluraler] + 1;
					} else {
						resultsre[pluraler] = 1;
					}
				} else {
					if (results.find(pluraler) != results.end()) {
							resultsre[pluraler] = resultsre[pluraler] + 1;
					} else {
						//cout << "booooom" << endl;
						pluraler = pluraler2;
						resultsre[pluraler] = resultsre[pluraler] + 1;
					}

				}
			}
		}
	}
	for (map <string, int>::iterator it = resultsre.begin() ; it != resultsre.end() ; it++) {
		cout << "\"" << it->first << "\" ---- " << it->second << endl;
	}
	return 0;
}