#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <iomanip>
#include <stack>
#include <deque>
#include <queue>
#include <string>
#include <cstring>
#include <cstdio>
#include <algorithm>
#include <cmath>
#include <unordered_set>
#include <unordered_map>
#include <numeric>
#include <cassert>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef unsigned long long ull;
typedef pair<ll, ll> pll;
typedef pair<ld, ld> pld;
typedef pair<int, int> pii;

const ll INF = 1e18 + 1;
const int INF_INT = 2e9 + 100;
const ll POLTOS = 50;
const ll SOTKA = 100;
const ll P = 1e9 + 7;
const ld eps = 1e-11;
const ll base = 1000 * 1000 * 1000;
const ll M = 3050;
const ll MAXN = 1e6 + 1;
const ll MAXN1 = 2 * 1e2;
const ll N = 300;
const ld pi = acos(-1);
const int mod = 1e9 + 7;
const int S_M = 20;
const ll LEN = 96;
const ll DMOD = 500000004;
const ll PR = 32000;

#define eb emplace_back
#define pb push_back
#define pf push_front
#define ppb pop_back
#define ppf pop_front
#define mp make_pair
#define ALL(a) (a).begin(), (a).end()
#define sqr(x) ((x) * (x))
#define x first
#define y second

ll t[MAXN * 4];

void build(ll a[], int left, int right, int v) {
    if (right - left == 1) {
        t[v] = a[left];
        return;
    }
    int mid = (left + right) / 2;
    build(a, left, mid, v * 2 + 1);
    build(a, mid, right, v * 2 + 2);
    t[v] = t[v * 2  + 1] + t[v * 2 + 2];
}

ll get_sum(int v, int left, int right, int tree_left, int tree_right) {
    if (left >= tree_right || tree_left >= right) {
        return 0;
    }
    if (tree_left >= left && tree_right <= right) {
        return t[v];
    }
    int mid = (tree_left + tree_right) / 2;
    return get_sum(v * 2 + 1, left, right, tree_left, mid) + get_sum(v * 2 + 2, left, right, mid, tree_right);
}


void update(int v, int pos, int key, int tree_left, int tree_right) {
    if (tree_right - tree_left == 1) {
        t[v] = key;
        return;
    }

    int mid = (tree_right + tree_left) / 2;

    if (pos < mid) {
        update(v * 2 + 1, pos, key, tree_left, mid);
    } else {
        update(v * 2 + 2, pos, key, mid, tree_right);
    }

    t[v] = t[v * 2 + 1] + t[v * 2 + 2];
}

int main() {
    int n;
    cin >> n;
    ll a[n];

    for (int i = 0; i < n; ++i) {
        cin >> a[i];
    }

    build(a, 0, n, 0);
    string arg;
    while (cin >> arg) {
        if (arg == "sum") {
            int left, right;
            cin >> left >> right;
            left--;
            cout << get_sum(0, left, right, 0, n) << endl;
        } else {
            int pos, key;
            cin >> pos >> key;
            pos--;
            update(0, pos, key, 0, n);
        }
    }

    return 0;
}