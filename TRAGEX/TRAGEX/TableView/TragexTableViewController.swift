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
    
    var optionsList = [TravelBanchmark]()
    let clipsEnv = CreateEnvironment()
    let filePath = Bundle.main.path(forResource: "combination", ofType: "clp", inDirectory: "Rules")
    

    override func viewDidLoad() {
        super.viewDidLoad()
        
        EnvLoad(clipsEnv,filePath);
//        EnvWatch(clipsEnv,"activations");
//        EnvWatch(clipsEnv,"facts");
        
        self.optionsList = [
            TravelBanchmark(name: "A", options:[Criteria(name: "1A",checked: false), Criteria(name: "2A",checked: true)]),
            TravelBanchmark(name: "B", options:[Criteria(name: "1B",checked: true), Criteria(name: "2B",checked: false)]),
            TravelBanchmark(name: "C", options:[Criteria(name: "1C",checked: false), Criteria(name: "2C",checked: true)])
        ]
        
        // Invoca i metodi che ricaricano i dati
        self.tableView.reloadData()

        // Uncomment the following line to preserve selection between presentations
        // self.clearsSelectionOnViewWillAppear = false

        // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
        // self.navigationItem.rightBarButtonItem = self.editButtonItem
    }
    
    
    @IBAction func start(_ sender: Any) {
        print("cavolo")
        EnvReset(clipsEnv);

//        EnvAssertString(clipsEnv,"(permutation (values madonna)")
        EnvRun(clipsEnv,-1);
        
        EnvFocus(clipsEnv, EnvFindDefmodule(clipsEnv, "TRIP"))
        let expression = "(find-all-facts ((?f trip)) TRUE)";
//        let expression = "(find-all-facts ((?f travel-banchmark)) TRUE)";
        var outputValue: DATA_OBJECT = DATA_OBJECT.init();
        EnvEval(clipsEnv, expression, &outputValue)
        
        Function.init().isUserModelValid(clipsEnv!, result: outputValue)
        print("fino a qua")
        
    }
    
    override func viewWillAppear(_ animated:Bool) {
        super.viewWillAppear(animated)
//        print(self.optionsList)
        tableView.reloadData()
    }

    // MARK: - Table view data source

    override func numberOfSections(in tableView: UITableView) -> Int {
        // #warning Incomplete implementation, return the number of sections
        return 1
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        // #warning Incomplete implementation, return the number of rows
        return self.optionsList.count
    }

    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "optionCell", for: indexPath)
        
        let travelBanchmark = self.optionsList[indexPath.row]
        
        //Testo a sx
        cell.textLabel?.text = travelBanchmark.name
        
        //Testo a dx (checked deve essere a true)
        if let option = travelBanchmark.options.first(where: {$0.checked}) {
            cell.detailTextLabel?.text = option.name
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
        self.performSegue(withIdentifier: "selectOptionSegue", sender: tableView)
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
        
        let optionsTableViewController = segue.destination as! OptionsDetailTableViewController
        
        let travelBanchmark = self.optionsList[self.tableView.indexPathForSelectedRow!.row]
        
        optionsTableViewController.delegate = self
        optionsTableViewController.title = travelBanchmark.name
        optionsTableViewController.criteriaList = travelBanchmark.options
        // Get the new view controller using segue.destination.
        // Pass the selected object to the new view controller.
    }
    

}
