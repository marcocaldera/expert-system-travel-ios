//
//  Trip.swift
//  TRAGEX
//
//  Created by Marco Caldera on 24/09/2019.
//  Copyright © 2019 Marco Caldera. All rights reserved.
//

import Foundation

//struct Trip {
//    let certainties: Float
//    let resortSequence: [String]
//    let placeSequence: [String]
//    let pricePerNight: [Int]
//    let daysDistributions: [Int]
//}

//class Trip: NSObject {
//    @objc let certainties: Float
//    @objc let resortSequence: [String]
//    @objc let placeSequence: [String]
//    @objc let pricePerNight: [Int]
//    @objc let daysDistributions: [Int]
//}


@objcMembers class Trip: NSObject {
    var certainties: Float = 0.0
    var resortSequence: [String] = []
    var placeSequence: [String] = []
    var pricePerNight: [Int] = []
    var daysDistributions: [Int] = []
}
