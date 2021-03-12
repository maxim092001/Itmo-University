#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <string>
#include <set>

namespace {

    bool comparator(const std::string &a, const std::string &b) {
        return a < b;
    }

    bool comparator_ignore_case(const std::string &a, const std::string &b) {
        for (size_t i = 0; i < std::min(a.size(), b.size()); ++i) {
            if (tolower(a[i]) < tolower(b[i])) {
                return true;
            } else if (tolower(a[i]) > tolower(b[i])) {
                return false;
            }
        }
        return a.size() < b.size();
    }

    bool isblank_string(const std::string &s) {
        return std::count_if(s.begin(), s.end(), [](unsigned char c) { return std::isblank(c); }) == (int) s.size();
    }

    bool isnumber(const std::string &s) {
        size_t index = 0;

        while (index < s.size() && isblank(s[index])) {
            index++;
        }

        if (index >= s.size() && !isdigit(s[index]) && s[index] != '-' && s[index] != '+') {
            return false;
        }

        if (!isdigit(s[index])) {
            index++;
        }

        for (; index < s.size(); index++) {
            if (isdigit(s[index]) == 0) {
                return false;
            }
        }
        return true;
    }


    bool comparator_numeric(const std::string &a, const std::string &b) {
        if (isblank_string(a)) {
            return isnumber(b) ? std::stoi(b) >= 0 : true;
        }

        if (isblank_string(b)) {
            return isnumber(a) ? std::stoi(a) < 0 : false;
        }

        if (isnumber(a) && isnumber(b)) {
            return std::stoi(a) < std::stod(b);
        }

        if (isnumber(a)) {
            return std::stoi(a) <= 0;
        }

        if (isnumber(b)) {
            return 0 <= std::stoi(b);
        }

        return a < b;

    }
}
int main(int argc, char **argv) {
    std::ifstream file(argv[argc - 1]);

    std::vector<std::string> strings;
    std::set<std::string> keys;

    std::string temp;
    while (getline(file, temp)) {
        strings.emplace_back(temp);
    }

    for (int i = 0; i < argc - 1; i++) {
        keys.insert(argv[i]);
    }

    if (keys.find("-f") != keys.end() || keys.find("--ignore-case") != keys.end()) {
        sort(strings.begin(), strings.end(), comparator_ignore_case);
    } else if (keys.find("-nf") != keys.end() || keys.find("-n") != keys.end() ||
               keys.find("--numeric-sort") != keys.end()) {
        sort(strings.begin(), strings.end(), comparator_numeric);
    } else {
        sort(strings.begin(), strings.end(), comparator);
    }

    for (auto &string : strings) {
        std::cout << string << "\n";
    }
    return 0;
}