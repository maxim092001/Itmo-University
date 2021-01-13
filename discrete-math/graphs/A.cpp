#include <iostream>
#include <deque>
#include <vector>
#include <algorithm>
#include <unordered_set>
 
using namespace std;
 
 
int main() {
    int n;
    cin >> n;
 
    deque<int> q;
    bool g[n + 1][n + 1];
    vector<int> colours(n + 1);
 
    for (int i = 2; i <= n; i++) {
        string s;
        cin >> s;
        for (int j = 0; j < s.size(); j++) {
            g[i][j + 1] = g[j + 1][i] = s[j] == '1';
        }
    }
 
    for (int i = 1; i < n + 1; i++) {
        q.push_back(i);
    }
 
    for (int i = 0; i < n * (n - 1); i++) {
        auto ptr_second = q.begin() + 1;
        if (!g[*ptr_second][*q.begin()]) {
            int begin_element = *q.begin();
            auto cnt1 = q.begin() + 3;
            auto cnt2 = q.begin() + 4;
            while (!g[*cnt1][begin_element] || !g[*cnt2][*ptr_second]) {
                cnt1++;
                cnt2 += (cnt2 != q.end());
            }
            reverse(ptr_second, ++cnt1);
        }
        q.push_back(q.front());
        q.pop_front();
    }
 
    while (!q.empty()) {
        cout << q.front() << " ";
        q.pop_front();
    }
}