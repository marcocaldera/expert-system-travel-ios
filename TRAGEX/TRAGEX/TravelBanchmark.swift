//
//  TravelBanchmark.swift
//  TRAGEX
//
//  Created by Marco Caldera on 22/09/2019.
//  Copyright © 2019 Marco Caldera. All rights reserved.
//

import Foundation

struct TravelBanchmark {
    let name: String
    var options: [Criteria]
}

struct Criteria {
    let name: String
    var checked: Bool
}
