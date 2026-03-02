module pika;


// ============================================================
// PikaQueue - min-heap based priority queue
// Matches Julia's PikaQueue exactly.
// Uses 0-based indexing internally but clause IDs are 0-based too.
// Heap is stored in q[0..n-1].
// ============================================================

fn PikaQueue pq_new(int capacity) {
    PikaQueue pq;
    pq.n = 0;
    pq.q = mem::new_array(int, (usz)capacity);
    pq.p = mem::new_array(bool, (usz)capacity);
    for (int i = 0; i < capacity; i++) {
        pq.q[i] = 0;
        pq.p[i] = false;
    }
    return pq;
}

fn PikaQueue pq_full(int capacity) {
    PikaQueue pq;
    pq.n = capacity;
    pq.q = mem::new_array(int, (usz)capacity);
    pq.p = mem::new_array(bool, (usz)capacity);
    for (int i = 0; i < capacity; i++) {
        pq.q[i] = i;  // 0-based clause IDs
        pq.p[i] = true;
    }
    return pq;
}

fn bool PikaQueue.is_empty(&self) {
    return self.n == 0;
}

fn void pq_swap(PikaQueue* q, int i, int j) {
    int tmp = q.q[i];
    q.q[i] = q.q[j];
    q.q[j] = tmp;
}

fn int PikaQueue.pop(PikaQueue* self) {
    int ret = self.q[0];
    self.q[0] = self.q[self.n - 1];
    self.n -= 1;
    self.p[ret] = false;

    // bubble down (0-indexed)
    int i = 0;
    while (true) {
        int l_idx = 2 * i + 1;
        int r_idx = 2 * i + 2;
        if (r_idx < self.n) {
            int l = self.q[l_idx];
            int r = self.q[r_idx];
            if (l < r) {
                if (self.q[i] <= l) break;
                pq_swap(self, i, l_idx);
                i = l_idx;
            } else {
                if (self.q[i] <= r) break;
                pq_swap(self, i, r_idx);
                i = r_idx;
            }
            continue;
        } else if (l_idx < self.n) {
            if (self.q[i] > self.q[l_idx]) {
                pq_swap(self, i, l_idx);
            }
        }
        break;
    }

    return ret;
}

fn void PikaQueue.push(PikaQueue* self, int x) {
    if (self.p[x]) return;
    self.p[x] = true;
    self.q[self.n] = x;
    self.n += 1;

    // bubble up
    int i = self.n - 1;
    while (i > 0) {
        int parent = (i - 1) / 2;
        if (self.q[parent] <= self.q[i]) break;
        pq_swap(self, parent, i);
        i = parent;
    }
}

fn void PikaQueue.reset_with_sorted(PikaQueue* self, int[] sorted_vals) {
    self.n = (int)sorted_vals.len;
    for (int i = 0; i < (int)self.p.len; i++) {
        self.p[i] = false;
    }
    for (int i = 0; i < (int)sorted_vals.len; i++) {
        self.q[i] = sorted_vals[i];
        self.p[sorted_vals[i]] = true;
    }
}

fn void PikaQueue.reset_from(PikaQueue* self, PikaQueue* other) {
    self.n = other.n;
    for (int i = 0; i < (int)other.q.len; i++) {
        self.q[i] = other.q[i];
    }
    for (int i = 0; i < (int)other.p.len; i++) {
        self.p[i] = other.p[i];
    }
}

fn void PikaQueue.free(PikaQueue* self) {
    mem::free(self.q.ptr);
    mem::free(self.p.ptr);
    self.n = 0;
}
