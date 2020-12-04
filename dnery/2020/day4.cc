#include <algorithm>
#include <iostream>
#include <cctype>
#include <string>
#include <vector>
#include <map>

using namespace std;

bool check_byr(const map<string,string>& reg);
bool check_iyr(const map<string,string>& reg);
bool check_eyr(const map<string,string>& reg);
bool check_hgt(const map<string,string>& reg);
bool check_hcl(const map<string,string>& reg);
bool check_ecl(const map<string,string>& reg);
bool check_pid(const map<string,string>& reg);

bool is_whitespace_only(const string& line)
{
    auto it = line.begin();
    while (isspace(*it)) { ++it; }
    if (it != line.end()) { return false; }
    return true;
}

void consume_string(map<string,string>& reg, string line)
{
    // Reduce white space consecutive occurrences to 1
    auto new_end = unique(line.begin(), line.end(), [](const char& a, const char& b) {
        return isspace(a) and isspace(b);
    });

    // Left strip, right strip
    auto new_beg = line.begin();
    if (isspace(*new_beg)) { ++new_beg; }
    if (isspace(*new_end)) { --new_end; }

    line.erase(new_end, line.end());
    line.erase(line.begin(), new_beg);

    long cbeg = 0;
    long cmid = 0;
    long cend = 0;

    while (cend != line.size())
    {
        cend = min(line.find(' ', cbeg), line.size());

        // Parse the key:value
        cmid = line.find(':', cbeg);
        string ckey = line.substr(cbeg, cmid - cbeg);
        string cval = line.substr(cmid + 1, cend - (cmid + 1));
        if (ckey.compare("cid") != 0) { reg[ckey] = cval; } // Rule: ignore 'cid'

        cbeg = cend + 1;
    }
}

bool check_reg1(const map<string,string>& reg)
{
    if (reg.size() < 7) { return false; }
    return true;
}

bool check_reg2(const map<string,string>& reg)
{
    /*
        byr (Birth Year) - four digits; at least 1920 and at most 2002.
        iyr (Issue Year) - four digits; at least 2010 and at most 2020.
        eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
        hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
        hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
        ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
        pid (Passport ID) - a nine-digit number, including leading zeroes.
        cid (Country ID) - ignored, missing or not.
     */

    vector<bool (*)(const map<string,string>&)> funcs{
        check_byr,
        check_iyr,
        check_eyr,
        check_hgt,
        check_hcl,
        check_ecl,
        check_pid};

    for (auto& checker_func: funcs) {
        bool ret = checker_func(reg);
        if (!ret) { return false; }
    }

    return true;
}

int main()
{
    string line;
    long valid_maps = 0;
    map<string,string> reg;

    while (getline(cin, line))
    {
        // If a white space only line is found,
        // proceed to the next registry/entry
        if (is_whitespace_only(line)) {
            //valid_maps += int(check_reg1(reg)); // part 1
            valid_maps += int(check_reg2(reg));   // part 2
            reg.clear();
        }
        consume_string(reg, line);
    }

    cout << valid_maps << endl;
}

bool check_byr(const map<string,string>& reg)
{
    auto it = reg.find("byr");
    if (it == reg.end()) { return false; }

    if (it->second.size() != 4) { return false;}

    int value = stoi(it->second);
    if (value < 1920 || value > 2002) { return false; }

    return true;
}
bool check_iyr(const map<string,string>& reg)
{
    auto it = reg.find("iyr");
    if (it == reg.end()) { return false; }

    if (it->second.size() != 4) { return false;}

    int value = stoi(it->second);
    if (value < 2010 || value > 2020) { return false; }

    return true;
}
bool check_eyr(const map<string,string>& reg)
{
    auto it = reg.find("eyr");
    if (it == reg.end()) { return false; }

    if (it->second.size() != 4) { return false;}

    int value = stoi(it->second);
    if (value < 2020 || value > 2030) { return false; }

    return true;
}
bool check_hgt(const map<string,string>& reg)
{
    auto it = reg.find("hgt");
    if (it == reg.end()) { return false; }

    if (it->second.size() == 4 && it->second.compare(it->second.size() - 2, 2, "in") == 0)
    {
        int value = stoi(it->second.substr(0, 2));
        if (value < 59 || value > 76) { return false; }
    }
    else if (it->second.size() == 5 && it->second.compare(it->second.size() - 2, 2, "cm") == 0)
    {
        int value = stoi(it->second.substr(0, 3));
        if (value < 150 || value > 193) { return false; }
    }
    else { return false; }

    return true;
}
bool check_hcl(const map<string,string>& reg)
{
    auto it = reg.find("hcl");
    if (it == reg.end()) { return false; }

    int nchars = 6;
    int val_it = 0;
    if (it->second[val_it++] != '#' ) { return false; }
    while (nchars > 0)
    {
        if (!isdigit(it->second[val_it]))
        {
            if (it->second.find_first_of("abcdef", val_it) == string::npos) { return false; }
        }
        val_it++;
        nchars--;
    }

    return true;
}
bool check_ecl(const map<string,string>& reg)
{
    auto it = reg.find("ecl");
    if (it == reg.end()) { return false; }

    vector<string> colors{ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };
    auto color = colors.begin();

    while ((*color).compare(it->second) != 0 && color != colors.end()) { color++; }
    if (color == colors.end()) { return false; }
    return true;
}
bool check_pid(const map<string,string>& reg)
{
    auto it = reg.find("pid");
    if (it == reg.end()) { return false; }

    if (it->second.size() != 9) { return false; }
    for (auto ch = it->second.begin(); ch != it->second.end(); ++ch)
    {
        if (!isdigit(*ch)) { return false; }
    }

    return true;
}

