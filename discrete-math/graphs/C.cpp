#define _FORTIFY_SOURCE 0
#pragma GCC optimize("Ofast")
#pragma GCC optimize("no-stack-protector")
#pragma GCC optimize("unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,popcnt,abm,mmx,tune=native")
#pragma GCC optimize("fast-math")
 
#include <iostream>
#include <deque>
#include <algorithm>
#include <random>
 
using namespace std;
 
int main() {
    int n;
    cin >> n;
 
    vector<int> v;
 
    v.emplace_back(1);
 
    for (int i = 2; i <= n; i++) {
        int left = -1;
        int right = v.size();
        while (right - left > 1) {
            int mid = (right + left ) / 2;
            cout << 1 << " " << v[mid] << " " << i << endl;
            string res;
            cin >> res;
            if (res == "YES") {
                left = mid;
            } else {
                right = mid;
            }
        }
        v.insert(v.begin() + right, i);
    }
 
    cout << 0 << " ";
    for (int i : v) {
        cout << i << " ";
    }
}