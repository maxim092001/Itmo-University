#define _FORTIFY_SOURCE 0
#pragma GCC optimize("Ofast")
#pragma GCC optimize("no-stack-protector")
#pragma GCC optimize("unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,popcnt,abm,mmx,tune=native")
#pragma GCC optimize("fast-math")
 
#include <iostream>
#include <deque>
#include <algorithm>
#include <unordered_set>
#include <random>
 
using namespace std;
 
const int MAXN = 10000;
bool g[MAXN][MAXN];
 
bool hamilton(const deque<int> &q) {
    for (int i = 0; i < q.size(); i++) {
        if (!g[q.at(i)][q.at((i + 1) % q.size())]) {
            return false;
        }
    }
    return true;
}
 
int main() {
    int n;
    cin >> n;
 
    deque<int> q;
    for (int i = 1; i < n; i++) {
        string s;
        cin >> s;
        for (int j = 0; j < s.size(); j++) {
            g[i][j] = g[j][i] = s[j] == '1';
        }
    }
 
    for (int i = 0; i < n; i++) {
        q.push_back(i);
    }
 
    std::random_device rd;
    std::mt19937 mt(rd());
 
    while (true) {
        for (int i = 0; i < n * (n - 1); i++) {
            auto ptr_second = q.begin() + 1;
            if (!g[*ptr_second][*q.begin()]) {
                int begin_element = *q.begin();
                auto cnt1 = q.begin() + 3;
                auto cnt2 = q.begin() + 4;
                while ((cnt1 != q.end() && !g[*cnt1][begin_element]) || (cnt2 != q.end() && !g[*cnt2][*ptr_second])) {
                    cnt1++;
                    cnt2 += (cnt2 != q.end());
                }
                reverse(ptr_second, ++cnt1);
            }
            q.push_back(q.front());
            q.pop_front();
        }
 
        if (hamilton(q)) {
            for (int i : q) {
                cout << i + 1 << ' ';
            }
            break;
        } else {
            random_shuffle(q.begin(), q.end());
        }
    }
}