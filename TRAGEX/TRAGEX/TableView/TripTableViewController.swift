//
//  TripTableViewController.swift
//  TRAGEX
//
//  Created by Marco Caldera on 25/09/2019.
//  Copyright © 2019 Marco Caldera. All rights reserved.
//

import UIKit

class TripTableViewController: UITableViewController {
    
    var infoList = Trip()

    override func viewDidLoad() {
        super.viewDidLoad()

        self.tableView.allowsSelection = false
//        self.infoList = ["Cao", "come", "va"]
//        self.tableView.reloadData()
        // Uncomment the following line to preserve selection between presentations
        // self.clearsSelectionOnViewWillAppear = false

        // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
        // self.navigationItem.rightBarButtonItem = self.editButtonItem
    }

    // MARK: - Table view data source

    override func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        switch section {
        case 0:
            return "Tappe"
        case 1:
            return "Report"
        default:
            return nil
        }
//        return nil
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        // #warning Incomplete implementation, return the number of sections
        return 2
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        // #warning Incomplete implementation, return the number of rows
        return section == 0 ? self.infoList.placeSequence.count : 3
    }

    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        if indexPath.section == 0 {
            let cell = tableView.dequeueReusableCell(withIdentifier: "tripPlace", for: indexPath)
            cell.textLabel?.text = self.infoList.placeSequence[indexPath.row]
            cell.detailTextLabel?.text = self.infoList.resortSequence[indexPath.row] + " (" + String(self.infoList.pricePerNight[indexPath.row]) + "€)"
            let label = UILabel.init(frame: CGRect(x:0,y:0,width:10,height:20))
            label.text = String(self.infoList.daysDistributions[indexPath.row])
            label.textColor = UIColor.systemBlue
            label.font = UIFont(name:"HelveticaNeue-Bold", size: 16.0)
            cell.accessoryView = label
            
            return cell
            
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: "reportCell", for: indexPath)
            
            switch indexPath.row {
            case 0:
                cell.textLabel?.text = "Certainty"
                cell.detailTextLabel?.text = String(format:"%.2d", Int((self.infoList.certainties * 100).rounded(.up)))+"%"
            case 1:
                cell.textLabel?.text = "N° di giorni"
                cell.detailTextLabel?.text = String(self.infoList.daysDistributions.reduce(0, +))
            case 2:
                cell.textLabel?.text = "Costo totale"
                cell.detailTextLabel?.text = String(self.infoList.pricePerNight.reduce(0, +)) + "€"
            default:
                break
            }
            
            return cell
        }
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

    /*
    // Override to support conditional rearranging of the table view.
    override func tableView(_ tableView: UITableView, canMoveRowAt indexPath: IndexPath) -> Bool {
        // Return false if you do not want the item to be re-orderable.
        return true
    }
    */

    /*
    // MARK: - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        // Get the new view controller using segue.destination.
        // Pass the selected object to the new view controller.
    }
    */

}
