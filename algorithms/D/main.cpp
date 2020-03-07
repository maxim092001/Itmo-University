#include <iostream>
#include <algorithm>

using namespace std;

typedef long long ll;

const ll MAXN = 1e6 + 1;

struct Node {
    int val = 0;
    int d = -1;
    int ans = 0;
    int left_bit = 0;
    int right_bit = 0;
};

struct Query {
    int left;
    int right;
    int col;
} q[MAXN];

int mxRight = -1;
Node t[MAXN * 4];

int trueValue(int v, int left, int right) {
    if (t[v].d == 0) {
        return 0;
    } else if (t[v].d == 1) {
        return right - left;
    } else
        return t[v].val;
}

int trueLeftBit(int v) {
    if (t[v].d != -1) {
        return t[v].d;
    }

    return t[v].left_bit;
}

int trueAns(int v) {
    if (t[v].d != -1) {
        return t[v].d;
    }

    return t[v].ans;
}

int trueRightBit(int v) {
    if (t[v].d != -1) {
        return t[v].d;
    }

    return t[v].right_bit;
}

void push(int v) {
    if (t[v].d == -1) {
        return;
    }

    t[v * 2 + 1].d = t[v * 2 + 2].d = t[v].d;
    t[v].d = -1;
}

void update_on_segment(int v, int left, int right, int set_value, int tree_left, int tree_right) {
    if (tree_left >= left && tree_right <= right) {
        t[v].d = set_value;
        return;
    }

    if (tree_left >= right || tree_right <= left) {
        return;
    }

    int mid = (tree_right + tree_left) / 2;
    push(v);
    update_on_segment(v * 2 + 1, left, right, set_value, tree_left, mid);
    update_on_segment(v * 2 + 2, left, right, set_value, mid, tree_right);

    t[v].val = trueValue(v * 2 + 1, tree_left, mid) + trueValue(v * 2 + 2, mid, tree_right);
    t[v].left_bit = trueLeftBit(v * 2 + 1);
    t[v].right_bit = trueRightBit(v * 2 + 2);

    if (trueRightBit(v * 2 + 1) == trueLeftBit(v * 2 + 2) && trueRightBit(v * 2 + 1) == 1) {
        t[v].ans = trueAns(v * 2 + 1) + trueAns(v * 2 + 2) - 1;
    } else {
        t[v].ans = trueAns(v * 2 + 1) + trueAns(v * 2 + 2);
    }
}


int findPower(int x) {
    int p = 1;
    while (p < x) {
        p *= 2;
    }
    return p * 2;
}

int main() {
      freopen("input.txt", "r", stdin);
      freopen("output.txt", "w", stdout);

    int n;
    cin >> n;
    int minNegative = 0;

    for (int i = 0; i < n; ++i) {
        char color;
        int left, len;
        cin >> color >> left >> len;

        q[i].col = color == 'W' ? 0 : 1;
        q[i].left = left;
        q[i].right = left + len;

        minNegative = min(minNegative, q[i].left);
        mxRight = max(mxRight, q[i].right);
    }

    if (minNegative < 0) {
        for (int i = 0; i < n; ++i) {
            q[i].left -= minNegative;
            q[i].right -= minNegative;
            mxRight = max(mxRight, q[i].right);
        }
    }

    int treeSize = findPower(mxRight);

    for (int i = 0; i < n; ++i) {
        update_on_segment(0, q[i].left, q[i].right, q[i].col, 0, treeSize);
        cout << t[0].ans << " " << t[0].val << endl;
    }

    return 0;
}
