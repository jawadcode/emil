macro_rules! bail {
    () => { println!() };
    ($($arg:tt)*) => { println!($($arg)*) };
}
