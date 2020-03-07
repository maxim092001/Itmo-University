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

int r;

struct event {
    int x;
    int y1;
    int y2;
    int type;

    event(int x, int y1, int y2, int type) {
        this->x = x;
        this->y1 = y1;
        this->y2 = y2;
        this->type = type;
    }
};

struct node {
    int value;
    int add = 0;
};

vector<event> a;
node t[4 * MAXN];

bool cmp(event a, event b) {
    if (a.x == b.x) {
        return a.type > b.type;
    }

    return a.x < b.x;
}

void push(int v) {
    if (t[v].add == 0) {
        return;
    }

    t[v * 2 + 1].add += t[v].add;
    t[v * 2 + 2].add += t[v].add;
    t[v].value += t[v].add;
    t[v].add = 0;
}


void add_on_segment(int qleft, int qright, int x, int v, int left, int right) {
    push(v);

    if (left >= qleft && right <= qright) {
        t[v].add += x;
        push(v);
        return;
    }

    if (right < qleft || left > qright) {
        return;
    }

    int mid = (left + right) / 2;

    add_on_segment(qleft, qright, x, v * 2 + 1, left, mid);
    add_on_segment(qleft, qright, x, v * 2 + 2, mid + 1, right);

    t[v].value = max(t[v * 2 + 1].value, t[v * 2 + 2].value);
}

int query_max(int v, int l, int r, int qleft, int qright) {
    push(v);

    if (l >= qleft && r <= qright) {
        return t[v].value;
    }

    if (r < qleft || l > qright) {
        return -INF_INT;
    }

    int mid = (l + r) / 2;
    push(v);
    int s1 = query_max(v * 2 + 1, l, mid, qleft, qright);
    int s2 = query_max(v * 2 + 2, mid + 1, r, qleft, qright);
    return max(s1, s2);
}

int find_max(int qleft, int qright, int value) {
    if (qleft == qright) {
        return qleft;
    }

    int mid = (qleft + qright) / 2;
    if (query_max(0, 0, r, qleft, mid) == value) {
        find_max(qleft, mid, value);
    } else {
        find_max(mid + 1, qright, value);
    }
}

int main() {


    // freopen("input.txt", "r", stdin);
    // freopen("output.txt", "w", stdout);

    int n;
    cin >> n;

    int minNegativeX = 0;
    int minNegativeY = 0;

    for (int i = 0; i < n; i++) {
        pair<int, int> leftUp;
        pair<int, int> rightDown;

        cin >> leftUp.first >> leftUp.second;
        cin >> rightDown.first >> rightDown.second;

        minNegativeX = min(minNegativeX, leftUp.first);
        minNegativeX = min(minNegativeX, rightDown.first);

        minNegativeY = min(minNegativeY, leftUp.second);
        minNegativeY = min(minNegativeY, rightDown.second);

        a.emplace_back(leftUp.first, rightDown.second, leftUp.second, 1);
        a.emplace_back(rightDown.first, rightDown.second, leftUp.second, -1);
    }

    for (auto &i : a) {
        i.y1 += abs(minNegativeY);
        i.y2 += abs(minNegativeY);
        i.x += abs(minNegativeX);
    }


    sort(a.begin(), a.end(), cmp);
    int ans = 0;
    int numOfEvent = 0;
    int ansY = 0;
    r = static_cast<int>(1e6 + 100) + minNegativeY;

    for (int i = 0; i < a.size(); i++) {
        event event = a[i];
        int left = min(event.y1, event.y2);
        int right = max(event.y1, event.y2);
        if (event.type == 1) {
            add_on_segment(left, right, 1, 0, 0, r);
        } else {
            add_on_segment(left, right, -1, 0, 0, r);
        }
        int query = query_max(0, 0, r, 0, r);
        if (query > ans) {
            ans = query;
            numOfEvent = i;
            ansY = find_max(0, r, query);
        }
    }
    cout << ans << endl;
    cout << a[numOfEvent].x + minNegativeX << " " << ansY + minNegativeY << endl;

    return 0;
}
