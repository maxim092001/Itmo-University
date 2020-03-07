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
const ll MAXN = 1e6 + 150;
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

int s[MAXN][25];
int lg[MAXN];
int a[MAXN];
int v[MAXN], u[MAXN];

int ans(int le, int ri)
{
    if (le == ri) {
        return a[le];
    }
    int k = lg[ri - le + 1];
    return min(s[le][k], s[ri - (1 << k) + 1][k]);
}

int main()
{
    int n, m;
    cin >> n >> m;
    cin >> a[0];


    for (int i = 0; i < n - 1; ++i) {
        a[i + 1] = (23 * a[i] + 21563) % 16714589;
    }

    lg[1] = 0;

    for(int i = 2; i <= n; ++i) {
        lg[i] = lg[i / 2] + 1;
    }

    for(int i = 0; i < n; ++i) {
        s[i][0] = a[i];
    }

    for(int j = 1; j <= lg[n] + 2; ++j) {
        for(int i = 0; i + (1 << j) <= n; ++i) {
            s[i][j] = min(s[i][j - 1], s[i + (1 << (j - 1))][j - 1]);
        }
    }

    ll u1, v1;
    cin >> u1 >> v1;
    ll r = 0;
    ll u, v;

    for (ll i = 1; i <= m; i++) {
        if (v1 > u1) {
            r = ans(u1 - 1, v1 - 1);
        } else {
            r = ans(v1 - 1, u1 - 1);
        }


        if (i < m) {
            u = ((17 * u1 + 751 + r + 2 * (i)) % n + 1);
            v = ((13 * v1 + 593 + r + 5 * (i)) % n + 1);
            u1 = u, v1 = v;
        }


    }
    cout << u1 << " " << v1 << " " << r << endl;
    return 0;
}

