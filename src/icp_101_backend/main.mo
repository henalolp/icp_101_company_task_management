import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Time "mo:base/Time";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";

actor {
  // Define types for Task, Employee, and Company
  type Task = {
    nameOfTask: Text;
    id: Text;
    description: Text;
  };

  type Employee = {
    name: Text;
    id: Text;
    tasks: [Task];
    salary: Nat64;
    rank: Text;
  };

  type Company = {
    name: Text;
    owner: Principal;
    employees: [Employee];
    createdAt: Time.Time;
  };

  // Initialize HashMaps to store companies and employees
  let companies = HashMap.HashMap<Text, Company>(0, Text.equal, Text.hash);
  let employeesRecord = HashMap.HashMap<Text, Employee>(1, Text.equal, Text.hash);

  // Greet function for testing
  public query func greet(name: Text): async Text {
    return "Hello, " # name # "!";
  };

  // Register a new company
  public shared ({caller}) func create_company(name: Text): async Result.Result<Text, Text> {
    switch (companies.get(name)) {
      case (null) {
        let newCompany: Company = {
          name = name;
          owner = caller;
          employees = [];
          createdAt = Time.now();
        };
        companies.put(name, newCompany);
        return #ok("Company created successfully");
      };
      case (?_) {
        return #err("Company already exists");
      }
    }
  };

  // Add an employee to a company
  public shared ({caller}) func add_employee(companyName: Text, employeeName: Text, salary: Nat64, rank: Text): async Result.Result<Text, Text> {
    switch (companies.get(companyName)) {
      case (null) {
        return #err("Company not found");
      };
      case (?company) {
        if (!Principal.equal(caller, company.owner)) {
          return #err("Only the owner is allowed to add employees");
        };
        
        let employeeId = Int.toText(Time.now());
        let newEmployee: Employee = {
          id = employeeId;
          name = employeeName;
          salary = salary;
          rank = rank;
          tasks = [];
        };

        company.employees := Array.append(company.employees, [newEmployee]);
        companies.put(companyName, company);
        employeesRecord.put(employeeId, newEmployee);

        return #ok("Employee added successfully");
      }
    }
  };

  // Get details of a specific employee by ID
  public query func get_employee_details(employeeId: Text): async Result.Result<Employee, Text> {
    switch (employeesRecord.get(employeeId)) {
      case (null) {
        return #err("Employee not found");
      };
      case (?employee) {
        return #ok(employee);
      }
    }
  };

  // Remove an employee from a company
  public shared ({caller}) func remove_employee(employeeId: Text, companyName: Text): async Result.Result<Text, Text> {
    switch (companies.get(companyName)) {
      case (null) {
        return #err("Company not found");
      };
      case (?company) {
        if (!Principal.equal(caller, company.owner)) {
          return #err("Only the owner can remove employees");
        };

        let updatedEmployees = Array.filter(company.employees, func (e) { e.id != employeeId });
        company.employees := updatedEmployees;
        companies.put(companyName, company);
        employeesRecord.delete(employeeId);

        return #ok("Employee removed successfully");
      }
    }
  };

  // Get a list of all employees in a company
  public query func get_all_employees(companyName: Text): async Result.Result<[Employee], Text> {
    switch (companies.get(companyName)) {
      case (null) {
        return #err("Company not found");
      };
      case (?company) {
        return #ok(company.employees);
      }
    }
  };

  // Assign a task to an employee
  public shared ({caller}) func assign_tasks(companyName: Text, employeeId: Text, taskName: Text, description: Text): async Result.Result<Text, Text> {
    switch (companies.get(companyName)) {
      case (null) {
        return #err("Company not found");
      };
      case (?_) {
        switch (employeesRecord.get(employeeId)) {
          case (null) {
            return #err("Employee not found");
          };
          case (?employee) {
            let taskId = Int.toText(Time.now());
            let newTask: Task = {
              id = taskId;
              nameOfTask = taskName;
              description = description;
            };

            employee.tasks := Array.append(employee.tasks, [newTask]);
            employeesRecord.put(employeeId, employee);

            return #ok("Task assigned successfully");
          }
        }
      }
    }
  }
};
