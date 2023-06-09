use anyhow::Context;
use std::{
    collections::{HashSet, VecDeque},
    fmt::{Display, Write},
    rc::Rc,
    str::FromStr,
};

pub(crate) mod data;

use crate::data::*;

const EXIT_LANE: u8 = 2;
const GRID_SIZE: usize = 6;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct GameState {
    vehicles: Vec<Vehicle>,
}

impl Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut matrix = [[0; GRID_SIZE]; GRID_SIZE];

        for (i, vehicle) in self.vehicles.iter().enumerate() {
            for (x, y) in vehicle.positions() {
                debug_assert!(x < 6, "vehicle({}) x out of bounds: {}", vehicle, x);
                debug_assert!(y < 6, "vehicle({}) y out of bounds: {}", vehicle, y);
                debug_assert_eq!(
                    0, matrix[y][x],
                    "collission in occupancy_matrix at ({},{})",
                    y, x
                );
                matrix[y][x] = i + 1;
            }
        }
        f.write_str("--------\n")?;
        let d = vec![
            ' ', 'R', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
        ];
        for row in matrix {
            f.write_char('|')?;
            for b in row {
                if b > 0 {
                    f.write_fmt(format_args!("{}", d[b]))?;
                } else {
                    f.write_char(' ')?;
                }
            }
            f.write_char('|')?;
            f.write_char('\n')?;
        }
        f.write_str("--------\n")
    }
}

impl GameState {
    pub fn occupancy_matrix(&self) -> [[bool; GRID_SIZE]; GRID_SIZE] {
        let mut matrix = [[false; GRID_SIZE]; GRID_SIZE];

        for vehicle in &self.vehicles {
            for (x, y) in vehicle.positions() {
                debug_assert!(x < GRID_SIZE, "vehicle({}) x out of bounds: {}", vehicle, x);
                debug_assert!(y < GRID_SIZE, "vehicle({}) y out of bounds: {}", vehicle, y);
                debug_assert!(
                    !matrix[y][x],
                    "collission in occupancy_matrix at ({},{})",
                    y, x
                );
                matrix[y][x] = true
            }
        }

        matrix
    }

    pub fn is_solved(&self) -> bool {
        let red_car = self
            .vehicles
            .iter()
            .filter(|&v| v.kind == VehicleKind::RedCar)
            .collect::<Vec<_>>();
        debug_assert_eq!(
            red_car.len(),
            1,
            "failed asserting that only one red car exists in the game state"
        );
        let red_car = *red_car.get(0).unwrap();

        let matrix = self.occupancy_matrix();
        debug_assert_eq!(
            red_car.orientation,
            Orientation::Horizontal,
            "failed asserting that the red car is in a horizontal position"
        );

        debug_assert_eq!(
            red_car.pos.1, EXIT_LANE,
            "failed asserting that the red car is in the correct lane"
        );

        let start_x = (red_car.pos.0 + 2) as usize;
        (start_x..GRID_SIZE).all(|x| !matrix[EXIT_LANE as usize][x])
    }
}

impl FromStr for GameState {
    type Err = anyhow::Error;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        str.chars()
            .collect::<Vec<char>>()
            .chunks(4)
            .map(|chunk| {
                chunk
                    .try_into()
                    .context("failed to split string into chunks of 4")
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|v| {
                let mut vv = v;
                vv.sort();
                GameState { vehicles: vv }
            })
    }
}

fn solve(state: GameState) -> Option<(u32, Vec<GameState>)> {
    let mut seen: HashSet<Rc<GameState>> = HashSet::new();
    let mut buffer: VecDeque<(u32, Rc<GameState>, Vec<Rc<GameState>>)> = VecDeque::new();

    let state = Rc::new(state);
    seen.insert(state.clone());
    buffer.push_back((0, state, Vec::new()));

    type SearchEntry = (u32, Rc<GameState>, Vec<Rc<GameState>>);

    // this method returns true if the location is blocked by another car (and thus we cannot continue)
    #[inline]
    fn evaluate_loc(
        matrix: &[[bool; 6]; 6],
        (x, y): (usize, usize),
        entry: &SearchEntry,
        vehicle: &Vehicle,
        seen: &mut HashSet<Rc<GameState>>,
        buffer: &mut VecDeque<SearchEntry>,
    ) -> bool {
        // if the car were te be placed in (x, y) would that be possible?
        // first we check all the coordinates of the car
        for (x, y) in vehicle.copy_with_xy(x as u8, y as u8).positions() {
            if matrix[y][x] {
                return true;
            }
        }
        let (moves, current_state, history) = entry;
        let mut new_vehicles = current_state.vehicles.clone();
        new_vehicles.iter_mut().for_each(|v| {
            if v == vehicle {
                *v = vehicle.copy_with_xy(x as u8, y as u8);
            }
        });

        let new_state = GameState {
            vehicles: new_vehicles,
        };

        // if we've already been here but with less moves we can skip adding this new location to the buffer
        if seen.contains(&new_state) {
            return false;
        }

        let new_state = Rc::new(new_state);
        seen.insert(new_state.clone());

        let mut new_history = history.to_vec();
        new_history.push(new_state.clone());

        buffer.push_back((moves + 1, new_state, new_history));

        false
    }

    while let Some(entry) = buffer.pop_front() {
        let (moves, ref current_state, ref history) = entry;
        if current_state.is_solved() {
            // we add one extra to the moves
            return Some((
                moves + 1,
                history
                    .iter()
                    .map(|s| s.as_ref().clone())
                    .collect::<Vec<_>>(),
            ));
        }

        for vehicle in current_state.vehicles.iter() {
            // Todo: creating this version of the occupancy matrix is probably slow as hell
            // the point of this is to create a collision matrix for the vehicle currently being
            // moved that does not include the current vehicle
            let mut matrix = current_state.occupancy_matrix();
            for (x, y) in vehicle.positions() {
                matrix[y][x] = false;
            }

            let (cur_x, cur_y) = (vehicle.pos.0 as usize, vehicle.pos.1 as usize);
            let len = vehicle.kind.len();

            if vehicle.orientation == Orientation::Horizontal {
                // move left
                for new_x in (0..cur_x).rev() {
                    if evaluate_loc(
                        &matrix,
                        (new_x, cur_y),
                        &entry,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    ) {
                        break;
                    }
                }

                // move right
                for new_x in (cur_x + 1)..=(GRID_SIZE - len) {
                    if evaluate_loc(
                        &matrix,
                        (new_x, cur_y),
                        &entry,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    ) {
                        break;
                    }
                }
            } else {
                // move up
                for new_y in (0..cur_y).rev() {
                    if evaluate_loc(
                        &matrix,
                        (cur_x, new_y),
                        &entry,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    ) {
                        break;
                    }
                }

                // move down
                for new_y in (cur_y + 1)..=(GRID_SIZE - len) {
                    if evaluate_loc(
                        &matrix,
                        (cur_x, new_y),
                        &entry,
                        vehicle,
                        &mut seen,
                        &mut buffer,
                    ) {
                        break;
                    }
                }
            }
        }
    }

    None
}

fn main() -> anyhow::Result<()> {
    let state = "R02HC30VC40HC22VC32VC52VC04VT14HC43VC54V".parse::<GameState>()?;
    println!("initial state: \n{}", state);

    if let Some((solution, history)) = solve(state) {
        println!("solveable in {} moves", solution);
        for (n, state) in history.iter().enumerate() {
            println!("Move {}\n{}", n + 1, state);
        }
        println!("solveable in {} moves", solution)
    } else {
        println!("puzzle has no solution");
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_that_order_does_not_change_state() {
        let state1 = "R22HT55V".parse::<GameState>().unwrap();
        let state2 = "T55VR22H".parse::<GameState>().unwrap();

        assert_eq!(state1, state2);
    }

    #[test]
    fn test_that_different_states_are_not_equal() {
        let state1 = "R22HT55V".parse::<GameState>().unwrap();
        let state2 = "R22HT54V".parse::<GameState>().unwrap();

        assert_ne!(state1, state2);
    }
}
