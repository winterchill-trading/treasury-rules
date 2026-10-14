include Id.Make (struct
    let module_name = "Treasury_rules.Rule_id"
    let what = "rule id"
    let forbidden = [ ':' ]
  end)
