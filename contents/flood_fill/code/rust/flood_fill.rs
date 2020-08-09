use ndarray::prelude::*; // 0.13.1
use std::collections::VecDeque;

fn color(canvas: &mut Array<usize, Ix2>, loc: &[usize; 2], old_val: usize, new_val: usize) {
    // do not color a nonexistent pixel
    if let Some(pixel) = canvas.get(*loc) {
        // only change color if the element value is the old value
        if pixel == &old_val {
            canvas[*loc] = new_val;
        }
    }
}

fn find_neighbors(canvas: &Array<usize, Ix2>, loc: &[usize; 2], val: usize) -> Vec<[usize; 2]> {
    // find neighbors
    let mut possible_neighbors: Vec<[usize; 2]> = vec![[loc[0] + 1, loc[1]], [loc[0], loc[1] + 1]];
    // guard against underflow errors
    if loc[0] as isize > 0 {
        possible_neighbors.push([loc[0] - 1, loc[1]]);
    }
    if loc[1] as isize > 0 {
        possible_neighbors.push([loc[0], loc[1] - 1]);
    }

    // exclude neighbors that are out of bounds and are not the old fill value
    possible_neighbors
        .into_iter()
        .filter(|i| canvas.get(*i) == Some(&val))
        .collect()
}

fn stack_fill(canvas: &mut Array<usize, Ix2>, loc: &[usize; 2], old_val: usize, new_val: usize) {
    // don't do anything if old_val and new_val are identical
    if new_val == old_val {
        return;
    }

    // set up the stack
    let mut stack: Vec<[usize; 2]> = Vec::new();
    stack.push(*loc);

    while let Some(current_loc) = stack.pop() {
        if canvas[current_loc] == old_val {
            color(canvas, &current_loc, old_val, new_val);
            let neighbors = find_neighbors(canvas, &current_loc, old_val);
            stack.extend(neighbors);
        }
    }
}

fn queue_fill(canvas: &mut Array<usize, Ix2>, loc: &[usize; 2], old_val: usize, new_val: usize) {
    // don't do anything if old_val and new_val are identical
    if new_val == old_val {
        return;
    }

    // set up the queue
    let mut queue: VecDeque<[usize; 2]> = VecDeque::new();
    queue.push_back(*loc);

    // color the initial location
    color(canvas, &loc, old_val, new_val);

    while let Some(current_loc) = queue.pop_front() {
        let up_next = find_neighbors(canvas, &current_loc, old_val);
        // color as we enqueue neighbors
        for n in up_next {
            color(canvas, &n, old_val, new_val);
            queue.push_back(n);
        }
    }
}

fn recursive_fill(
    canvas: &mut Array<usize, Ix2>,
    loc: &[usize; 2],
    old_val: usize,
    new_val: usize,
) {
    // don't do anything if old_val and new_val are identical
    if new_val == old_val {
        return;
    }

    color(canvas, &loc, old_val, new_val);

    let up_next = find_neighbors(canvas, &loc, old_val);
    up_next
        .iter()
        .for_each(|i| recursive_fill(canvas, i, old_val, new_val));
}

fn main() {
    let grid = array![
        [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0]
    ];

    let solution_grid = array![
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0]
    ];

    let start_loc = [0, 0];

    let mut recursive_grid = grid.clone();
    recursive_fill(&mut recursive_grid, &start_loc, 0, 1);

    let mut queue_grid = grid.clone();
    queue_fill(&mut queue_grid, &start_loc, 0, 1);

    let mut stack_grid = grid.clone();
    stack_fill(&mut stack_grid, &start_loc, 0, 1);

    println!("Starting grid:");
    println!("{}", grid);
    println!();
    println!("Solution grid:");
    println!("{}", solution_grid);
    println!();
    println!(
        "Recursive grid {} correct.",
        if recursive_grid == solution_grid {
            "is"
        } else {
            "is NOT"
        }
    );
    println!(
        "Queue grid {} correct.",
        if queue_grid == solution_grid {
            "is"
        } else {
            "is NOT"
        }
    );
    println!(
        "Stack grid {} correct.",
        if stack_grid == solution_grid {
            "is"
        } else {
            "is NOT"
        }
    );
}
