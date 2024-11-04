import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Time "mo:base/Time";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";
import Buffer "mo:base/Buffer";
actor {
  type Task={
    nameoftask:Text;
    id:Text;
    description:Text;
  };
  type Employee={
    name:Text;
    id:Text;
    tasks:[Task];
    salary:Nat64;
    rank:Text
  };
  type Company={
    name:Text;
    owner:Principal;
    employess:[Employee];
    created_at:Time.Time;
  };


  type Result<Ok,Err> =Result.Result<Ok,Err>;


  let companies =HashMap.HashMap<Text,Company>(0,Text.equal,Text.hash);
  let employessrecord=HashMap.HashMap<Text,Employee>(1,Text.equal,Text.hash);

  public query func greet(name : Text) : async Text {
    return "Hello, " # name # "!";
  };

  //registe compay

  public shared ({caller}) func create_company(name:Text):async Result.Result<Text,Text>{
    //check if the name of the company is already taken

    switch(companies.get(name)){
      case (null){
        //register company

        let new_company:Company={
          name=name;
          owner=caller;
          employess=[];
          created_at=Time.now();
        };

        companies.put(name,new_company);
        return #ok("company created successfully")
      };
      case(?_found){
        return #err("company already exists")
      }
    }
  };
  //add employees

   public shared ({caller}) func add_employee(companyname:Text,nameofemployee:Text,salary:Nat64,rank:Text):async Result.Result<Text,Text>{

    //check if the comapny exists

    switch(companies.get(companyname)){
      case (null){
        return #err("company not found");
      };
      case(?found){
        //check if its the owner performing the action
         if (Principal.equal(caller, found.owner)) {
          //add employee
           let id:Text=Int.toText(Time.now());
          let new_employee:Employee={
            id=id;
            name=nameofemployee;
            salary=salary;
            rank=rank;
            tasks=[];
          };

          //update comapnies data

          //firts convert the employess array to buffer
          let employeesBuffer=Buffer.fromArray<Employee>(found.employess);

          employeesBuffer.add(new_employee);
          
          //reconvert employees buffer to array
          let updatedemployeearray=Buffer.toArray(employeesBuffer);

          //uuupdate the company data
          let update_company:Company={
            owner=found.owner;
            name=found.name;
            created_at=found.created_at;
            employess=updatedemployeearray;
          };

          //update hashmap
          companies.put(found.name,update_company);
           employessrecord.put(id,new_employee);
         
          return #ok("successfully added employee");

            } else {
                return #err("only owner is allowed to perform this action");
            }
      }
    };
   };
    //get an employess
    public query func get_employee_details(employeeid:Text):async Result.Result<Employee,Text>{
      //verify if the  exists
      switch( employessrecord.get(employeeid)){
        case(null){
          return #err("employee not found");
        };
        case(?found){
          return #ok(found)
        }
      }
    };
    //remove an employee

    public shared ({caller}) func remove_employee(employeeid:Text,companyname:Text):async Result.Result<Text,Text>{

      //check if the comapny exists
      switch(companies.get(companyname)){
        case(null){
          return #err("compny ith given name not found");
        };
        case(?found){

          //verify its the owner perfroming the action
            if (Principal.equal(caller, found.owner)) {
          //covert employess array to buffer inorder to mapfilter it
             let bufferArray=Buffer.fromArray<Employee>(found.employess);

             let newbuffer=Buffer.mapFilter<Employee,Employee>(bufferArray,func(x){

              if(x.id==employeeid){
                 null;
              }else{
                ?x
              }
             });
             //reconvert newbbuffer to array
             let newarray=Buffer.toArray(newbuffer);
             //update the companies data
               let update_company:Company={
                 owner=found.owner;
                 name=found.name;
                  created_at=found.created_at;
                 employess=newarray;
              };
                companies.put(found.name,update_company);
                 employessrecord.delete(employeeid);
             return #ok("employee removed successfully");
            };
            return #err("fas");
        }
      }
    };
    //get all employess
    public query func get_all_employees(companyname:Text):async Result.Result<[Employee],Text>{

      //verify if the company exists
      switch(companies.get(companyname)){
        case(null){
          return #err("company does not exists");
        };
        case(?found){
          return #ok(found.employess)
        }
      }
    };
     //assigns tasks to employess
     public shared ({caller}) func assign_tasks(companyname:Text,employeeid:Text,nameoftask:Text,description:Text):async Result.Result<Text,Text>{
         //verify if the company exists
      switch(companies.get(companyname)){
        case(null){
          return #err("company does not exists");
        };
        case(?_found){
          //verify if employee exists
           switch(employessrecord.get(employeeid)){
            case (null){
              return #err("employee not found");
            };
            case(?employee){
              //create a new task
               let id:Text=Int.toText(Time.now());
              let new_task:Task={
                id;
                nameoftask;
                 description

              };

              let employeeBuffer=Buffer.fromArray<Task>(employee.tasks);
              employeeBuffer.add(new_task);
              let update_employee=Buffer.toArray(employeeBuffer);
                let updated:Employee={
                 id=employee.id;
                 name=employee.name;
                 salary=employee.salary;
                 rank=employee.rank;
                 tasks=update_employee;
               };

              //update employee status
               employessrecord.put(id,updated);
        
               return #ok("assigned tasks successfuly");

            }
           }
         
        }
      }

     }
   }
  //remove employess
  //assign tasks to employees
  //reasigns tasks to employeess
  //get all employeess
  //get an employess

