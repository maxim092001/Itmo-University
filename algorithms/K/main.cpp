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

int t[4 * MAXN];
int n;

void build(ll a[], int left, int right, int v) {
    if (right - left == 1) {
        t[v] = a[left];
        return;
    }
    int mid = (left + right) / 2;
    build(a, left, mid, v * 2 + 1);
    build(a, mid, right, v * 2 + 2);
    t[v] = min(t[v * 2 + 1], t[v * 2 + 2]);
}

ll get_min(int v, int left, int right, int tree_left, int tree_right) {
    if (left >= tree_right || tree_left >= right) {
        return INF_INT;
    }
    if (tree_left >= left && tree_right <= right) {
        return t[v];
    }
    int mid = (tree_left + tree_right) / 2;
    return min(get_min(v * 2 + 1, left, right, tree_left, mid),
               get_min(v * 2 + 2, left, right, mid, tree_right));
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

    t[v] = min(t[v * 2 + 1], t[v * 2 + 2]);
}

int get_nearest_zero_position(int left, int right) {
    if (right - left == 1) {
        return left;
    }

    int mid = (left + right) / 2;
    if (get_min(0, left, right, 0, n) != 0) {
        return INF_INT;
    } else {
        if (get_min(0, left, mid, 0, n) == 0) {
            return get_nearest_zero_position(left, mid);
        } else {
            return get_nearest_zero_position(mid, right);
        }
    }
}

int main() {


    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);
    int m;
    cin >> n >> m;

    for (int i = 0; i < m; ++i) {
        string query;
        cin >> query;

        int pos;
        cin >> pos;
        pos--;

        if (query == "exit") {
            update(0, pos, 0, 0, n);
        } else {
            int ans = get_nearest_zero_position(pos, n);
            if (ans == INF_INT) {
                ans = get_nearest_zero_position(0, pos + 1);
            }
            cout << ans + 1 << '\n';
            update(0, ans, 1, 0, n);
        }
    }
    return 0;
}
