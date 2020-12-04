#include<iostream>
#include<sstream>
#include<string>

using namespace std;

void split_into_buf(stringstream&, string);

int pw_is_valid_pol1(int imin, int imax, char ch, string line)
{
    int occurrences = 0;
    for (auto it = line.begin(); it != line.end(); ++it) { if (*it == ch) { occurrences++; } }
    if (occurrences < imin || occurrences > imax) { return 0; }
    return 1;
}

int pw_is_valid_pol2(int pos1, int pos2, char ch, string line)
{
    pos1--; pos2--;
    if ((pos1 < line.size() && line[pos1] == ch) &&
        (pos2 >= line.size() || line[pos2] != ch)) { return 1; }
    if ((pos2 < line.size() && line[pos2] == ch) &&
        (pos1 >= line.size() || line[pos1] != ch)) { return 1; }
    return 0;
}

int main()
{
    string line;
    stringstream buf;
    long valid_passwords = 0;

    while (getline(cin, line))
    {
        int     a;      // integer 1
        int     b;      // integer 2
        char    ch;     // character
        string  seq;    // pw string

        // Read the input
        line.push_back(' ');
        split_into_buf(buf, line);
        buf >> a >> b >> ch >> seq;

        // Do the thing
        // valid_passwords += pw_is_valid_pol1(a, b, ch, seq); // part 1
        valid_passwords += pw_is_valid_pol2(a, b, ch, seq); // part 2

        // Clear stream
        buf.str("");
    }

    cout << valid_passwords;

    return 0;
}

void split_into_buf(stringstream& fout, string line)
{
    const char* del = "-: ";

    size_t ibeg = 0;
    size_t iend = line.find_first_of(del);

    while (iend != string::npos)
    {
        if (iend - ibeg > 0)
        {
            fout << line.substr(ibeg, iend - ibeg) << " ";
        }
        ibeg = iend + 1;
        iend = line.find_first_of(del, ibeg);
    }
}
