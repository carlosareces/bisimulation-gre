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

// Returns number of targets in targetstr
int check_targets(vector <string> targets, string targetstr) {
	int ans = 0;
	for (int i = 0; i < targets.size() ; i++) {
		if (targetstr.find(targets[i]) != -1) ans++;
	}
	return ans;
}

int main() {

	string currentline = "";
	string currentexp = "";
	string targetstr = "";
	// The following are the targets of the experiment
	// In this version, we will allow n targets.
	vector <string> targets;
	//cilinder1,cilinder2,triangle1,ball1,rectangle1,
	//cucumber1,cuboid1
	targets.push_back("cilinder1");
	targets.push_back("cilinder2");
	targets.push_back("triangle1");
	targets.push_back("ball1");
	targets.push_back("rectangle1");
	targets.push_back("cucumber1");
	targets.push_back("cuboid1");
	
	int cases = 0;
	set <string> entities;
	map <string, int> resultsre;
	bool erplural = false;
	bool answerfound = false;
	// singulars contains the amount of singular ERs
	map <string, string> singulars;
	// doubles contains the amount of doubles ERs
	map < pair <string, string>, string > doubles;
	// triples contains the amount of triple ERs
	map < pair <string, pair <string, string> >, string> triples;
	// cuartets contains the amount of four-uples ERs
	set <string> cuartets;
	// Counts how many targets are in targetstr

	while (getline(cin, currentline)) {
		//cout << currentline << endl;
		if (currentline.find("--") != -1) {
			cases++;
			entities.clear();
			singulars.clear();
			doubles.clear();
			cuartets.clear();
			answerfound = false;
		} else if (currentline.find("T:") != -1) {
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
			// Until here, is only preprocessing of the entities of the model
		} else if (currentline.find(":") != -1) {
			// This line may be a referring expression
 			size_t pos = currentline.find(":");
 			currentexp = currentline.substr(0, pos);
			targetstr = currentline.substr(pos + 1,currentline.size()-1);
			int amount = check_targets(targets, targetstr);
			// cout << amount << endl;
			// Here I add the ER of only one element
			//cout << "se llamo a check_targets " << cases << " veces" << endl;
			//cout << "check_targets dio :" << amount << endl;
			if (amount == 1) {
				string tar = "";
				for (int i = 0 ; i < targets.size() ; i++) {
					if (targetstr.find(targets[i]) != -1) tar = targets[i];
				}
				//cout << "tenemos que tar es: " << tar << endl;
				string actualstr = targetstr;
				size_t tarpos = actualstr.find(tar);
				//cout << "actualstr es: " << actualstr << endl;
				//cout << "tenemos que tar es: " << tar << endl;
				actualstr = actualstr.replace(tarpos,tar.size(),"");
				//cout << "actualstr es: " << actualstr << endl;
				//cout << "check es: " << check(actualstr, entities) << endl;
				//cout << "currentexp es: " << currentexp << endl;
				if (check(actualstr, entities)) {
					//cout << "se introduce: " << currentexp << endl;
					if (singulars.find(tar) == singulars.end()) {
						singulars[tar] = currentexp;
					} else {
						//cout << "se introduce a ver: " << currentexp << endl;
						//cout << "si es mas chico que: " << singulars[tar] << endl;
						if (singulars[tar].size() > currentexp.size()) {
							singulars[tar] = currentexp;
						}
					}
				}
			} else if (amount == 2) {
				string tar1 = "", tar2 = "";
				for (int i = 0 ; i < targets.size() ; i++) {
					if (targetstr.find(targets[i]) != -1) {
						tar1 = targets[i];
						for (int j = 0 ; j != i && j < targets.size() ; j++) {
							if (targetstr.find(targets[j]) != -1) tar2 = targets[j];
						}
					}
				}
				string actualstr = targetstr;
				size_t tarpos1 = actualstr.find(tar1);
				actualstr = actualstr.replace(tarpos1,tar1.size(),"");
				size_t tarpos2 = actualstr.find(tar2);
				actualstr = actualstr.replace(tarpos2,tar2.size(),"");
				// Now I have in tar1 and tar2 the two targets in the string, let's remove them
				// to see if the expression is an ER
				if (check(actualstr, entities)) {
					if (doubles.find(make_pair(tar1,tar2)) == doubles.end()) {
						doubles[make_pair(tar1,tar2)] = currentexp;
					}
				}
			} else if (amount == 3) {
				string tar1 = "", tar2 = "", tar3 = "";
				set <string> tars;
				for (int i = 0 ; i < targets.size() ; i++) {
					tars.insert(targets[i]);
					if (targetstr.find(targets[i]) == -1) {
						tars.erase(targets[i]);
					}
				}
				set <string>::iterator it = tars.begin();
				tar1 = *it; it++;
				tar2 = *it; it++;
				tar3 = *it;
				string actualstr = targetstr;
				size_t tarpos1 = actualstr.find(tar1);
				actualstr = actualstr.replace(tarpos1,tar1.size(),"");
				size_t tarpos2 = actualstr.find(tar2);
				actualstr = actualstr.replace(tarpos2,tar2.size(),"");
				size_t tarpos3 = actualstr.find(tar2);
				actualstr = actualstr.replace(tarpos2,tar2.size(),"");
				pair <string, pair <string, string> > aux = make_pair(tar1, make_pair(tar2,tar3));
				if (triples.find(aux) == triples.end() && check(actualstr, entities)) {
					triples[aux] = currentexp;
				}

			} else if (amount = 4) {
				string actualstr = targetstr;
				for (int i = 0; i < targets.size() ; i++) {
					actualstr = actualstr.replace(actualstr.find(targets[i]), targets[i].size(),"");
				}
				if (check(actualstr, entities)) {
					cuartets.insert(currentexp);
				}
			}
			// As baseline, I only want to use singular ERs
			bool baseline = true;
			//bool baseline = false;
			if (!baseline) {
					if (cuartets.size() > 0 && !answerfound) {
					set <string>::iterator it = cuartets.begin();
					string aux = *it;
					answerfound = true;
					if (resultsre.find(aux) != resultsre.end()) {
						resultsre[aux] += 1;
					} else resultsre[aux] = 1;
				}
				if (triples.size() > 0 && !answerfound) {
					if (targets.size() == 4) {
						for (map < pair <string, pair <string, string> >, string>::iterator it = triples.begin(); it != triples.end() ; it++) {
							string tar1 = "", tar2 = "", tar3 = "";
							tar1 = it->first.first;
							tar2 = it->first.second.first;
							tar3 = it->first.second.second;
							for (map <string, string>::iterator itx = singulars.begin() ; itx != singulars.end() ; itx++) {
								if (itx->first != tar1 && itx->first != tar2 && itx->first != tar3) {
									string ans = it->second + " && " + itx->second;
									answerfound = true;
									if (resultsre.find(ans) != resultsre.end()) {
										resultsre[ans] += 1;
									} else resultsre[ans] = 1;
								}
							}			
						}
					} else if (targets.size() == 3) {
						for (map < pair <string, pair <string, string> >, string>::iterator it = triples.begin(); it != triples.end() ; it++) {
							string tar1 = "", tar2 = "", tar3 = "";
							tar1 = it->first.first;
							tar2 = it->first.second.first;
							tar3 = it->first.second.second;
							string ans = it->second;
							answerfound = true;
							if (resultsre.find(ans) != resultsre.end()) {
								resultsre[ans] += 1;
							} else resultsre[ans] = 1;
						}
					}
				}
				if (doubles.size() > 0 && !answerfound) {
					if (targets.size() == 4) {
						for (map < pair <string, string>, string>::iterator it = doubles.begin(); it != doubles.end() ; it++) {
							string tar1 = "", tar2 = "";
							tar1 = it->first.first;
							tar2 = it->first.second;
							for (map <string, string>::iterator itx = singulars.begin() ; itx != singulars.end() ; itx++) {
								for (map <string, string>::iterator ity = singulars.begin() ; ity != singulars.end() ; ity++) {
									if (itx->first != tar1 && itx->first != tar2 && ity->first != tar1 && ity->first != tar2 && itx->first != ity->first) {
										string ans = it->second + " && " + itx->second + " && " + ity->second;
										answerfound = true;
										if (resultsre.find(ans) != resultsre.end()) {
											resultsre[ans] += 1;
										} else resultsre[ans] = 1;
									}
								}
							}
						}
					} else if (targets.size() == 3) {
						for (map < pair <string, string>, string>::iterator it = doubles.begin(); it != doubles.end() ; it++) {
							string tar1 = "", tar2 = "";
							tar1 = it->first.first;
							tar2 = it->first.second;
							for (map <string, string>::iterator itx = singulars.begin() ; itx != singulars.end() ; itx++) {
								if (itx->first != tar1 && itx->first != tar2) {
									string ans = it->second + " && " + itx->second;
									answerfound = true;
									if (resultsre.find(ans) != resultsre.end()) {
										resultsre[ans] += 1;
									} else resultsre[ans] = 1;
								}
							}
						}
					} else if (targets.size() == 2) {
						for (map < pair <string, string>, string>::iterator it = doubles.begin(); it != doubles.end() ; it++) {
							string ans = "";
							ans = it->second;
							answerfound = true;
							if (resultsre.find(ans) != resultsre.end()) {
								resultsre[ans] += 1;
							} else resultsre[ans] = 1;
						}
					}
				} else if (!answerfound) {
					//cout <<"singulars size " << singulars.size() << endl;
					string ans = "";
					//cout << "size de singular: " << singulars.size() << endl;
					//cout << "size de targets: " << targets.size() << endl; 
					if (singulars.size() == targets.size()) {
						//sort(singulars.begin(), singulars.end());
						for (int i = 0 ; i < singulars.size() ; i++) {

							string aux = singulars[targets[i]];
							ans += aux;
							if (i != singulars.size() - 1) ans += " && ";
						}
						answerfound = true;
						if (resultsre.find(ans) != resultsre.end()) {
							resultsre[ans] += 1;
						} else resultsre[ans] = 1;
					}
				}
			} else {
				//cout <<"singulars size " << singulars.size() << endl;
				if (!answerfound) {
					string ans = "";
					//cout << "size de singular: " << singulars.size() << endl;
					//cout << "size de targets: " << targets.size() << endl; 
					if (singulars.size() == targets.size()) {
						//sort(singulars.begin(), singulars.end());
						for (int i = 0 ; i < singulars.size() ; i++) {

							string aux = singulars[targets[i]];
							ans += aux;
							if (i != singulars.size() - 1) ans += " && ";
						}
						answerfound = true;
						if (resultsre.find(ans) != resultsre.end()) {
							resultsre[ans] += 1;
						} else resultsre[ans] = 1;
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