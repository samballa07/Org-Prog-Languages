use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/**
    These traits are implemented for Nodes to make them comparable
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}

/**
    You must implement the above trait for the vector type
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {
        self.push(ele);
        let mut ele_index: i32 = self.len() as i32 - 1;
        let mut parent_index = (ele_index - 1) / 2;

        while self[ele_index as usize] < self[parent_index as usize] {
            self.swap(ele_index as usize, parent_index as usize);
            ele_index = parent_index;
            parent_index = (ele_index - 1) / 2;
        }
    }

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/
    fn dequeue(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }
        let end_ind = self.len() - 1;
        self.swap(0, end_ind);
        let ele = self.remove(end_ind);

        let mut parent = 0;
        let mut l_child = 2 * parent + 1;
        let mut r_child = 2 * parent + 2;

        while (r_child < self.len() && l_child < self.len() && parent < self.len())
            && (self[parent] >= self[l_child] || self[parent] >= self[l_child])
        {
            if self[parent] >= self[l_child] {
                if self[l_child] <= self[r_child] {
                    self.swap(parent, l_child);
                    parent = l_child;
                    l_child = 2 * parent + 1;
                } else {
                    self.swap(parent, r_child);
                    parent = r_child;
                    r_child = 2 * parent + 2;
                }
            } else if self[parent] >= self[r_child] {
                self.swap(parent, r_child);
                parent = r_child;
                r_child = 2 * parent + 2;
            }
        }
        return Some(ele);
    }

    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.is_empty() {
            return None;
        }
        return Some(&self[0]);
    }
}

/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32, i32), p2: (i32, i32)) -> i32 {
    let mut d1 = p2.0 - p1.0;
    d1 = d1.abs();
    let mut d2 = p2.1 - p1.1;
    d2 = d2.abs();
    return d1 + d2;
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn target_locator<'a>(
    allies: &'a HashMap<&String, (i32, i32)>,
    enemies: &'a HashMap<&String, (i32, i32)>,
) -> (&'a str, i32, i32) {
    let mut final_targets: HashMap<String, String> = HashMap::new();
    for (key, val) in enemies {
        let mut allies_match: HashMap<i32, String> = HashMap::new();
        let mut q: std::vec::Vec<i32> = Vec::new();
        for (akey, aval) in allies {
            let dist = distance(*val, *aval);
            allies_match.insert(dist, akey.to_string());
            q.enqueue(dist);
        }
        match q.peek() {
            Some(x) => match allies_match.get(x) {
                Some(value) => final_targets.insert(value.to_string(), key.to_string()),
                None => final_targets.insert(key.to_string(), key.to_string()),
            },
            None => final_targets.insert(key.to_string(), key.to_string()),
        };
    }
    let x = &final_targets[&"Stark".to_string()];
    for (key, val) in enemies {
        if key == &x {
            return (&key[..], val.0, val.1);
        }
    }
    return ("invalid", -1, -1);
}
