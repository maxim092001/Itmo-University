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

struct Query {
    ll left;
    ll right;
    ll value;
};

struct node {
    ll val = 0;
    ll set = INF;
};

Query q[MAXN];
node t[MAXN * 4];

bool cmp(const Query &a, const Query &b) {
    if ((a.value == b.value) && (a.left == b.left) && (a.right < b.right)) {
        return true;
    }

    if (a.value == b.value && a.left < b.left) {
        return true;
    }

    return a.value < b.value;
}

void push(int v) {
    if (t[v].set == INF) {
        return;
    }
    t[v * 2 + 1].set = t[v * 2 + 2].set = t[v].set;
    t[v].val = t[v].set;
    t[v].set = INF;
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
    push(v);
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


int main() {
    // freopen("input.txt", "r", stdin);
    // freopen("output.txt", "w", stdout);


    ll n, m;
    cin >> n >> m;

    for (int i = 0; i < m; i++) {
        ll left, right, value;
        cin >> left >> right >> value;
        left--;

        q[i].value = value;
        q[i].left = left;
        q[i].right = right;
    }

    sort(q, q + m, cmp);

    for (int i = 0; i < m; i++) {
        set_on_segment(0, q[i].left, q[i].right, q[i].value, 0, n);
    }

    for (int i = 0; i < m; i++) {
        if (q[i].value != query(0,  q[i].left, q[i].right, 0, n)) {
            return cout << "inconsistent", 0;
        }
    }


    cout << "consistent" << endl;
    for (int i = 0; i < n; i++) {
        cout << query(0, i, i + 1, 0, n) << " ";
    }
    return 0;
}
