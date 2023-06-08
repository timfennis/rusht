use anyhow::{anyhow, Context};
use std::{fmt::Display, str::FromStr};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Vehicle {
    pub pos: (u8, u8),
    pub orientation: Orientation,
    pub kind: VehicleKind,
}

impl Vehicle {
    pub fn copy_with_xy(&self, x: u8, y: u8) -> Self {
        Vehicle {
            pos: (x, y),
            orientation: self.orientation,
            kind: self.kind,
        }
    }

    pub fn positions(&self) -> Vec<(usize, usize)> {
        // todo: there must be a way to optimize this without using heap allocations
        let mut positions = Vec::new();
        let (x, y) = (self.pos.0 as usize, self.pos.1 as usize);

        let vlen = self.kind.len();

        for offset in 0..vlen {
            if self.orientation == Orientation::Horizontal {
                positions.push((x + offset, y));
            } else {
                positions.push((x, y + offset));
            }
        }
        positions
    }
}

impl Display for Vehicle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}{}{}{}",
            match self.kind {
                VehicleKind::Truck => 'T',
                VehicleKind::Car => 'C',
                VehicleKind::RedCar => 'R',
            },
            self.pos.0,
            self.pos.1,
            if self.orientation == Orientation::Horizontal {
                'H'
            } else {
                'V'
            }
        ))
    }
}

impl TryFrom<&[char]> for Vehicle {
    type Error = anyhow::Error;

    fn try_from(value: &[char]) -> Result<Self, Self::Error> {
        let value: [char; 4] = value.try_into()?;
        let kind = match value[0] {
            'R' => VehicleKind::RedCar,
            'C' => VehicleKind::Car,
            'T' => VehicleKind::Truck,
            _ => return Err(anyhow!("invalid vehicle type")),
        };

        let x = value[1]
            .to_digit(10)
            .map(|d| d as u8)
            .context("invalid x coordinate")?;
        let y = value[2]
            .to_digit(10)
            .map(|d| d as u8)
            .context("invalid y coordinate")?;

        let orientation = match value[3] {
            'H' => Orientation::Horizontal,
            'V' => Orientation::Vertical,
            _ => return Err(anyhow!("invalid or missing orientation")),
        };

        Ok(Vehicle {
            pos: (x, y),
            orientation,
            kind,
        })
    }
}

impl FromStr for Vehicle {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let chars: &[char] = &s.chars().collect::<Vec<char>>();
        chars.try_into()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VehicleKind {
    Truck,
    Car,
    RedCar,
}

impl VehicleKind {
    pub fn len(&self) -> usize {
        match self {
            VehicleKind::Truck => 3,
            VehicleKind::Car => 2,
            VehicleKind::RedCar => 2,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_that_a_vehicle_lists_its_coordinates_correctly() {
        assert_eq!(
            "T00H".parse::<Vehicle>().unwrap().positions(),
            vec![(0, 0), (1, 0), (2, 0)]
        );
        assert_eq!(
            "C00H".parse::<Vehicle>().unwrap().positions(),
            vec![(0, 0), (1, 0)]
        );
        assert_eq!(
            "T00V".parse::<Vehicle>().unwrap().positions(),
            vec![(0, 0), (0, 1), (0, 2)]
        );
        assert_eq!(
            "R00V".parse::<Vehicle>().unwrap().positions(),
            vec![(0, 0), (0, 1)]
        );
    }
}
