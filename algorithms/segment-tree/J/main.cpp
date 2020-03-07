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

const ll INF = 1e18 + 100;
const int INF_INT = 2e9 + 100;
const ll POLTOS = 50;
const ll SOTKA = 100;
const ll P = 1e9 + 7;
const ld eps = 1e-11;
const ll base = 1000 * 1000 * 1000;
const ll M = 3050;
const ll MAXN = 2e6 + 1;
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


struct node {
    ll val = 0;
    ll set = -INF;
};

int n;
node t[MAXN * 4];

void push(int v) {
    if (t[v].set == -INF) {
        return;
    }

    if (t[v * 2 + 1].set < t[v].set) {
        t[v * 2 + 1].set = t[v].set;
    }

    if (t[v * 2 + 2].set < t[v].set) {
        t[v * 2 + 2].set = t[v].set;
    }

    if (t[v].set > t[v].val) {
        t[v].val = t[v].set;
    }
    t[v].set = -INF;
}

void build(ll a[], int left, int right, int v) {
    if (right - left == 1) {
        t[v].val = a[left];
        return;
    }
    int mid = (left + right) / 2;
    build(a, left, mid, v * 2 + 1);
    build(a, mid, right, v * 2 + 2);
    t[v].val = min(t[v * 2 + 1].val, t[v * 2 + 2].val);
}

ll query_min(int v, int left, int right, int tree_left, int tree_right) {
    push(v);
    if (left >= tree_right || tree_left >= right) {
        return INF;
    }
    if (tree_left >= left && tree_right <= right) {
        return t[v].val;
    }
    int mid = (tree_left + tree_right) / 2;
    return min(query_min(v * 2 + 1, left, right, tree_left, mid),
               query_min(v * 2 + 2, left, right, mid, tree_right));
}

void set_on_segment(int v, int left, int right, ll set_value, int tree_left, int tree_right) {
    push(v);

    if (tree_left >= left && tree_right <= right) {
        t[v].set = set_value;
        push(v);
        return;
    }

    if (tree_left >= right || tree_right <= left) {
        return;
    }

    int mid = (tree_left + tree_right) / 2;
    set_on_segment(v * 2 + 1, left, right, set_value, tree_left, mid);
    set_on_segment(v * 2 + 2, left, right, set_value, mid, tree_right);
    t[v].val = min(t[v * 2 + 1].val, t[v * 2 + 2].val);
}


int find_min(int left, int right, int value) {
    if (left == right - 1) {
        return left;
    }

    int mid = (left + right) / 2;
    if (query_min(0, left, mid, 0, n) == value) {
        return find_min(left, mid, value);
    } else {
        return find_min(mid, right, value);
    }
}

int main() {
    // freopen("input.txt", "r", stdin);
    // freopen("output.txt", "w", stdout);


    int m;
    cin >> n >> m;

    for (int i = 0; i < m; i++) {
        string query;
        int left, right;

        cin >> query;
        if (query == "defend") {
            ll x;
            cin >> left >> right >> x;
            left--;
            set_on_segment(0, left, right, x, 0, n);
        }

        if (query == "attack") {
            cin >> left >> right;
            left--;
            ll query_ans = query_min(0, left, right, 0, n);
            cout << query_ans << " ";
            cout << find_min(left, right, query_ans) + 1  << endl;
        }
    }
    return 0;
}
