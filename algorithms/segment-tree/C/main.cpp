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
#define x first
#define y second

struct node {
    ll val;
    ll add = 0;
    ll set = INF;
};

node t[MAXN * 4];
ll a[MAXN];

void push(int v, int tree_left, int tree_right) {
    if (tree_left == tree_right) {
        if (t[v].add) {
            t[v].val += t[v].add;
            t[v].add = 0;
        } else if (t[v].set != INF) {
            t[v].val = t[v].set;
            t[v].set = INF;
        }

        return;
    }

    if (t[v].add) {
        if (t[v * 2 + 1].set != INF) {
            t[v * 2 + 1].set += t[v].add;
            t[v * 2 + 1].add = 0;
        } else if (t[v * 2 + 1].add) {
            t[v * 2 + 1].add += t[v].add;
            t[v * 2 + 1].set = INF;
        } else if(!t[v * 2 + 1].add && t[v * 2 + 1].set == INF) {
            t[v * 2 + 1].add = t[v].add;
            t[v * 2 + 1].set = INF;
        }

        if (t[v * 2 + 2].set != INF) {
            t[v * 2 + 2].set += t[v].add;
            t[v * 2 + 2].add = 0;
        } else if (t[v * 2 + 2].add) {
            t[v * 2 + 2].add += t[v].add;
            t[v * 2 + 2].set = INF;
        } else if(!t[v * 2 + 2].add && t[v * 2 + 2].set == INF) {
            t[v * 2 + 2].add = t[v].add;
            t[v * 2 + 2].set = INF;
        }

        t[v].val += t[v].add;
        t[v].add = 0;
        t[v].set = INF;
        return;
    }

    if (t[v].set != INF) {
        t[v * 2 + 1].set = t[v].set;
        t[v * 2 + 2].set = t[v].set;
        t[v * 2 + 1].add = 0;
        t[v * 2 + 2].add = 0;

        t[v].val = t[v].set;
        t[v].set = INF;
        t[v].add = 0;
        return;
    }
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

ll query(int v, int left, int right, int tree_left, int tree_right) {
    push(v, tree_left, tree_right);
    if (left >= tree_right || tree_left >= right) {
        return INF;
    }
    if (tree_left >= left && tree_right <= right) {
        return t[v].val;
    }
    int mid = (tree_left + tree_right) / 2;
//    push(v, tree_left, tree_right);
    return min(query(v * 2 + 1, left, right, tree_left, mid),
               query(v * 2 + 2, left, right, mid, tree_right));
}

void set_on_segment(int v, int left, int right, ll set_value, int tree_left, int tree_right) {
    push(v, tree_left, tree_right);

    if (tree_left >= left && tree_right <= right) {
        t[v].set = set_value;
        t[v].add = 0;
        push(v, tree_left, tree_right);
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


void add_on_segment(int v, int left, int right, ll add_value, int tree_left, int tree_right) {
    push(v, tree_left, tree_right);

    if (tree_left >= left && tree_right <= right) {
        t[v].add += add_value;
        t[v].set = INF;
        push(v, tree_left, tree_right);
        return;
    }

    if (tree_left >= right || tree_right <= left) {
        return;
    }

    int mid = (tree_left + tree_right) / 2;
    add_on_segment(v * 2 + 1, left, right, add_value, tree_left, mid);
    add_on_segment(v * 2 + 2, left, right, add_value, mid, tree_right);
    t[v].val = min(t[v * 2 + 1].val, t[v * 2 + 2].val);
}

int main() {
    // freopen("input.txt", "r", stdin);
    // freopen("output.txt", "w", stdout);

    int n;
    cin >> n;

    for (int i = 0; i < n; ++i) {
        cin >> a[i];
    }

    build(a, 0, n, 0);
    string arg;
    while (cin >> arg) {
        if (arg == "min") {
            ll left, right;
            cin >> left >> right;
            left--;
            cout << query(0, left, right, 0, n) << '\n';
        } else if (arg == "set") {
            ll left, right, val;
            cin >> left >> right >> val;
            left--;
            set_on_segment(0, left, right, val, 0, n);
        } else if (arg == "add") {
            ll left, right, val;
            cin >> left >> right >> val;
            left--;
            add_on_segment(0, left, right, val, 0, n);
        }
    }
    return 0;
}
