# Frontend

## Introduction

This is the frontend for DuoSwipe. The project uses Angular framework, has basic functionality of a website and is easy to install and use.

## Getting Started

### Installation

Clone DuoSwipe from GitHub to your desired location

- Open a Terminal window.
- Navigate to where you want the cloned directory to be.
- Type this line to the terminal window.
  ```
  git clone <https://github.com/thinhtun2000/duoswipe.git>
  ```
- Press `Enter` to create your local clone.

## Requirements

## Deployment instructions

### **Development server**

Run `ng serve` for a dev server. Navigate to `http://localhost:4200/`. The application will automatically reload if you change any of the source files.

### **Code scaffolding**

Run `ng generate component component-name` to generate a new component. You can also use `ng generate directive|pipe|service|class|guard|interface|enum|module`.

### **Build**

Run `ng build` to build the project. The build artifacts will be stored in the `dist/` directory.

### **Running unit tests**

Run `ng test` to execute the unit tests via [Karma](https://karma-runner.github.io/).

### **Further help**

To get more help on the Angular CLI use `ng help` or go check out the [Angular CLI Overview and Command Reference](https://angular.io/cli) page.

## Coding Conventions

### File Organization

The project consists of 4 base modules based on their usage:

1. **Core**

   Basic functionality of the app. Everything required for the app to run is placed here.

2. **Auth**

   Components that are authentication related. They can either grant access to or setup authentication for other components.

3. **User**

   Components that are accessible by all users, contain user-info related content.

4. **Shared**

   Reusable components that are shared between modules.

Each module is further divided into: components, guards, interceptors, layouts, models, pages, services, validators whichever is applied to the specific module.

Images are stored in assets. API base

### Naming Conventions

Regular TypeScript naming conventions:

- All name start with a letter
- Use camelCase for variable and function names
- Class names are capitalized

# Useful links & external services

## Angular framework

> https://angular.io/

## API docs

> https://github.com/SteeleConsulting/steele-frame/blob/dev/backend/README.md

## Third party packages & libraries

### Bootstrap docs

Bootstrap is a powerful, feature-packed frontend toolkit, provides the ability to build responsive sites.

> https://getbootstrap.com/docs/5.2/getting-started/introduction/

### Bootstrap icons

Free, high quality open source icon library. Can be used with or without Bootstrap.

> https://icons.getbootstrap.com/

### jQuery

jQuery simplifies HTML DOM traversal and manipulation, as well as event handling and CSS styling.

> https://jquery.com/

### popper.js

Position engine, is required if you are using Bootstrap.

> https://popper.js.org/docs/v2/

### PrimeNG

A rich set of open source native Angular UI components.

> https://www.primefaces.org/primeng/

### PrimeIcons

Free icon library from PrimeFaces.

> https://www.primefaces.org/primeng/icons
