/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    let mut x = 1;
    let mut count = 0;
    if n < 0 {
        return -1;
    }
    let sum = loop {
        if x == n {
            break count + n;
        }
        count += x;
        x = x + 1;
    };
    sum
}

/**
    Returns the number of elements in the list that
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut x: i32 = 0;
    for i in ls.iter() {
        if i <= &e && i >= &s {
            x += 1;
        }
    }
    return x;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for i in target.iter() {
        if !set.contains(i) {
            return false;
        }
    }
    return true;
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.is_empty() {
        return None;
    }
    let mut sum: f64 = 0.0;
    for i in ls.iter() {
        sum += i;
    }
    let mean = sum / ls.len() as f64;
    return Some(mean);
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut dec = 0;
    let two: i32 = 2;
    let mut count: i32 = (ls.len() as i32) - 1;
    for i in 0..ls.len() {
        if ls[i] == 1 {
            dec += two.pow(count as u32);
        }
        count = count - 1;
    }
    return dec;
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn is_prime(n: u32) -> bool {
    let mut x = n - 1;
    while x > 1 {
        if n % x == 0 {
            return false;
        }
        x -= 1;
    }
    return true;
}
pub fn next_prime(n: u32) -> u32 {
    let mut prime = n + 1;
    while !is_prime(prime) {
        prime += 1;
    }
    return prime;
}
pub fn factorize(n: u32) -> Vec<u32> {
    let mut prime = 2;
    let mut num = n;
    let mut list: Vec<u32> = Vec::new();
    while num > 1 {
        if num % prime == 0 {
            num = num / prime;
            list.push(prime);
        } else {
            prime = next_prime(prime);
        }
    }
    return list;
}

/**
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them,
    so the first becomes the last, the second becomes first, and so on.
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut list: Vec<i32> = Vec::new();
    for i in 0..lst.len() {
        if i == lst.len() - 1 {
            list.push(lst[0]);
        } else {
            list.push(lst[i + 1]);
        }
    }
    return list;
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if s == target {
        return true;
    } else if target.len() > s.len() {
        return false;
    }
    let mut end = target.len();
    let mut start = 0;
    let mut sub = &s[start..end];
    while end < s.len() {
        if sub == target {
            return true;
        } else {
            end += 1;
            start += 1;
            sub = &s[start..end];
        }
    }
    return false;
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s.is_empty() {
        return None;
    }
    let mut sub: &str = "";
    let mut start;
    for i in 0..s.len() {
        unsafe {
            start = i;
            let mut end = start + 1;
            let mut temp_sub = s.get_unchecked(start..end);
            loop {
                if s.get_unchecked(end..end + 1) != temp_sub {
                    break;
                } else {
                    end += 1;
                }
            }
            temp_sub = s.get_unchecked(start..end);
            if sub.len() < temp_sub.len() {
                sub = temp_sub
            }
        }
    }
    return Some(sub);
}
