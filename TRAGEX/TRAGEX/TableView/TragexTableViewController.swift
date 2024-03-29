//
//  TragexTableViewController.swift
//  TRAGEX
//
//  Created by Marco Caldera on 21/09/2019.
//  Copyright © 2019 Marco Caldera. All rights reserved.
//

import UIKit
import CLIPSiOS

class TragexTableViewController: UITableViewController {
        
    var bestTripList = [Trip]()
    var optionsList = [TravelBanchmark]()
    let clipsEnv = CreateEnvironment()
    let filePath = Bundle.main.path(forResource: "travel-agency_v2", ofType: "clp", inDirectory: "Rules")
    

    override func viewDidLoad() {
        super.viewDidLoad()
        
        EnvLoad(clipsEnv,filePath);
//        EnvWatch(clipsEnv,"activations");
//        EnvWatch(clipsEnv,"facts");
        
        self.optionsList = [
            TravelBanchmark(name: "Budget",
                            key: "travel-budget",
                            options:[
                                Criteria(name: "500",checked: true),
                                Criteria(name: "600",checked: false),
                                Criteria(name: "800",checked: false),
                                Criteria(name: "900",checked: false),
                                Criteria(name: "1500",checked: false)]
            ),
            TravelBanchmark(name: "N° di persone",
                            key: "people-number",
                            options:[
                                Criteria(name: "2",checked: true),
                                Criteria(name: "3",checked: false),
                                Criteria(name: "4",checked: false),
                                Criteria(name: "5",checked: false),
                                Criteria(name: "6",checked: false),
                                Criteria(name: "7",checked: false)]
            ),
            TravelBanchmark(name: "Durata viaggio",
                            key: "travel-duration",
                            options:[
                                Criteria(name: "3",checked: true),
                                Criteria(name: "4",checked: false),
                                Criteria(name: "5",checked: false),
                                Criteria(name: "6",checked: false)]
            ),
            TravelBanchmark(name: "N° minimo di stelle",
                            key: "min-resort-star",
                            options:[
                                Criteria(name: "unknown",checked: true),
                                Criteria(name: "2",checked: false),
                                Criteria(name: "3",checked: false),
                                Criteria(name: "4",checked: false)]
            ),
            TravelBanchmark(name: "N° di posti da visitare",
                            key: "number-of-place",
                            options:[
                                Criteria(name: "unknown",checked: true),
                                Criteria(name: "1",checked: false),
                                Criteria(name: "2",checked: false),
                                Criteria(name: "3",checked: false),
                                Criteria(name: "4",checked: false),
                                Criteria(name: "5",checked: false)]
            ),
            TravelBanchmark(name: "Regione preferita",
                            key: "favourite-region",
                            options:[
                                Criteria(name: "unknown",checked: true),
                                Criteria(name: "piemonte",checked: false),
                                Criteria(name: "liguria",checked: false),
                                Criteria(name: "lombardia",checked: false)]
            ),
            TravelBanchmark(name: "Tipologia viaggio",
                            key: "trip-type",
                            options:[
                                Criteria(name: "unknown",checked: true),
                                Criteria(name: "culturale",checked: false),
                                Criteria(name: "rilassante",checked: false)]
            ),
            TravelBanchmark(name: "Tratti personali",
                            key: "personal-trait",
                            options:[
                                Criteria(name: "unknown",checked: true),
                                Criteria(name: "avventura",checked: false),
                                Criteria(name: "comodità",checked: false)]
            )
        ]
//        
        // Invoca i metodi che ricaricano i dati
        self.tableView.reloadData()

        // Uncomment the following line to preserve selection between presentations
        // self.clearsSelectionOnViewWillAppear = false

        // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
        // self.navigationItem.rightBarButtonItem = self.editButtonItem
    }
    
    
    
    @IBAction func start(_ sender: Any) {
        
       
//        EnvReset(self.clipsEnv);
//        for item in self.optionsList {
//                    if let option = item.options.first(where: {$0.checked}) {
//                        EnvAssertString(
//                            self.clipsEnv,
//                            String(format: "(travel-banchmark (name %@) (value %@) (certainty 1.0))", item.key, option.name)
//                        )
//                    }
//                }
//
//        EnvRun(self.clipsEnv,-1);
//        EnvFocus(self.clipsEnv, EnvFindDefmodule(self.clipsEnv, "TRIP"))
//                let expression = "(find-all-facts ((?f trip)) TRUE)";
//
//                var outputValue: DATA_OBJECT = DATA_OBJECT.init();
//        EnvEval(self.clipsEnv, expression, &outputValue)
//        self.bestTripList = Function.init().isUserModelValid(self.clipsEnv!, result: outputValue) as! [Trip]
//        self.bestTripList = self.bestTripList.sorted(by: { $0.certainties > $1.certainties })
//        print("fatto")
//        self.tableView.reloadData()
        
        DispatchQueue.global(qos: .userInitiated).async {
            
            print("start")

//            EnvWatch(self.clipsEnv, "facts")
//            EnvWatch(self.clipsEnv, "rules")
//            EnvWatch(self.clipsEnv, "activations")

            EnvReset(self.clipsEnv);

            for item in self.optionsList {
                        if let option = item.options.first(where: {$0.checked}) {
                            EnvAssertString(
                                self.clipsEnv,
                                String(format: "(travel-banchmark (name %@) (value %@) (certainty 1.0))", item.key, option.name)
                            )
                        }
                    }

            EnvRun(self.clipsEnv,-1);

            EnvFocus(self.clipsEnv, EnvFindDefmodule(self.clipsEnv, "TRIP"))
                    let expression = "(find-all-facts ((?f trip)) TRUE)";

                    var outputValue: DATA_OBJECT = DATA_OBJECT.init();
            EnvEval(self.clipsEnv, expression, &outputValue)

            self.bestTripList = Function.init().isUserModelValid(self.clipsEnv!, result: outputValue) as! [Trip]
            self.bestTripList = self.bestTripList.sorted(by: { $0.certainties > $1.certainties })



            //        for trip in bestTripList {
            //            print(trip.certainties)
            //            print(trip.daysDistributions)
            //            print(trip.resortSequence)
            //            print(trip.placeSequence)
            //            print(trip.pricePerNight)
            //            print("---------------")
            //        }


            DispatchQueue.main.async {
                print("fatto")
                self.tableView.reloadData()
            }
        }
        
    }
    
    override func viewWillAppear(_ animated:Bool) {
        super.viewWillAppear(animated)
//        print(self.optionsList)
        tableView.reloadData()
    }

    // MARK: - Table view data source

    override func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        switch section {
        case 0:
            return "Criteria"
        case 1:
            if bestTripList.count != 0 {
                return "Trip"
            }
        default:
            return nil
        }
        return nil
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        
        return 2
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        // #warning Incomplete implementation, return the number of rows
        return section == 0 ? self.optionsList.count : self.bestTripList.count
    }

    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "optionCell", for: indexPath)
        
        if indexPath.section == 0 {
            let travelBanchmark = self.optionsList[indexPath.row]
            
            //Testo a sx
            cell.textLabel?.text = travelBanchmark.name
            
            //Testo a dx (checked deve essere a true)
            if let option = travelBanchmark.options.first(where: {$0.checked}) {
                cell.detailTextLabel?.text = option.name
                
                if travelBanchmark.key == "travel-budget" {
                    cell.detailTextLabel?.text = option.name + "€"
                }
                
            }
            
            cell.detailTextLabel?.textColor = UIColor.gray
            cell.detailTextLabel?.font = UIFont(name:"HelveticaNeue", size: 16.0)
            
            
        } else {
            let trip = self.bestTripList[indexPath.row]

            cell.textLabel?.text = trip.placeSequence.joined(separator:" ")
//            cell.detailTextLabel?.text = String(format:"%.2f", trip.certainties)
            cell.detailTextLabel?.text = String(format:"%.2d", Int((trip.certainties * 100).rounded(.up)))+"%"
            cell.detailTextLabel?.textColor = UIColor.systemBlue
            cell.detailTextLabel?.font = UIFont(name:"HelveticaNeue-Bold", size: 16.0)
            
//            cell.selectionStyle = .none
//            cell.backgroundColor = UIColor.clear
//            cell.textLabel?.textColor = UIColor.black
//            cell.textLabel?.text = "TEXT YOU WANT THE 'CELL' TO DISPLAY"
            
        }
        
        //Freccia indicatrice
        cell.accessoryType = UITableViewCell.AccessoryType.disclosureIndicator

        return cell
    }
 

    /*
    // Override to support conditional editing of the table view.
    override func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
        // Return false if you do not want the specified item to be editable.
        return true
    }
    */

    /*
    // Override to support editing the table view.
    override func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCell.EditingStyle, forRowAt indexPath: IndexPath) {
        if editingStyle == .delete {
            // Delete the row from the data source
            tableView.deleteRows(at: [indexPath], with: .fade)
        } else if editingStyle == .insert {
            // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
        }    
    }
    */

    /*
    // Override to support rearranging the table view.
    override func tableView(_ tableView: UITableView, moveRowAt fromIndexPath: IndexPath, to: IndexPath) {

    }
    */
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        
        if indexPath.section == 0 {
            self.performSegue(withIdentifier: "selectOptionSegue", sender: tableView)
        } else {
            self.performSegue(withIdentifier: "tripSegue", sender: tableView)
        }
    }

    /*
    // Override to support conditional rearranging of the table view.
    override func tableView(_ tableView: UITableView, canMoveRowAt indexPath: IndexPath) -> Bool {
        // Return false if you do not want the item to be re-orderable.
        return true
    }
    */

    
    // MARK: - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        
        if segue.identifier == "selectOptionSegue" {
            let optionsTableViewController = segue.destination as! OptionsDetailTableViewController
            
            let travelBanchmark = self.optionsList[self.tableView.indexPathForSelectedRow!.row]
            
            optionsTableViewController.delegate = self
            optionsTableViewController.title = travelBanchmark.name
            optionsTableViewController.criteriaList = travelBanchmark.options
            optionsTableViewController.criteriaIndex = self.tableView.indexPathForSelectedRow!.row
        } else {
            let tripTableViewController = segue.destination as! TripTableViewController
            tripTableViewController.title = self.bestTripList[self.tableView.indexPathForSelectedRow!.row].placeSequence.joined(separator:" ")
            tripTableViewController.infoList = self.bestTripList[self.tableView.indexPathForSelectedRow!.row]
        }
    }
    

}
