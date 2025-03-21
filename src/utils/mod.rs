pub fn vec_boxify<T>(list: Vec<T>) -> Vec<Box<T>> {
    list.into_iter().map(|x| Box::new(x)).collect()
}
