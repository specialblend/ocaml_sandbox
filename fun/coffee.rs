fn handle(cup: Cup) {
  let action = match cup {
      Some(coffee) => drink(coffee),
      None => cup.fill("☕️")
  };
  handle(action);
  sleep(Duration::SECOND);
}