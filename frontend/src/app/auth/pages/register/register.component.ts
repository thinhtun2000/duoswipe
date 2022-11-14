import { HttpErrorResponse } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { catchError, of } from 'rxjs';

import { RegisterRequest } from 'src/app/core/models/registerRequest';
import { RegisterResponse } from 'src/app/core/models/registerResponse';

import { User } from 'src/app/core/models/user';
import { AuthService } from 'src/app/core/services/auth/auth.service';
import { UserApiService } from 'src/app/core/services/user-api/user-api.service';
import { UserService } from 'src/app/core/services/user/user.service';

@Component({
  selector: 'app-register',
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.scss'],
})
export class RegisterComponent implements OnInit {
  public form: FormGroup;
  public submitting: boolean = false;

  myScriptElement: HTMLScriptElement;

  constructor(
    private fb: FormBuilder,
    private router: Router,
    private userApi: UserApiService,
    private userSvc: UserService,
    private authSvc: AuthService
  ) {
      this.myScriptElement = document.createElement("script");
      this.myScriptElement.src = "/assets/js/myjs.js";
      document.body.appendChild(this.myScriptElement);
  }

  ngOnInit(): void {
    this.form = this.fb.group({
      username: ['', [Validators.required]],
      email: ['', [Validators.required]],
      password: ['', [Validators.required]],
    });
  }

  public onSubmit(): void {
    const registerRequestObject = this.form.value as RegisterRequest;
    this.authSvc
      .register(registerRequestObject)
      .subscribe((response: RegisterResponse) => {
        switch (response.status) {
          case 'success':
            this.router.navigateByUrl('/login');
            console.log('register user success');
            break;
          default:
            console.log('register fail');
        }
      });
  }

  public changeVisibility(): void {
    var x = (document.getElementById('passwordInput') as HTMLInputElement).type;
    if (x === 'password')
      (document.getElementById('passwordInput') as HTMLInputElement).type =
        'text';
    else if (x === 'text')
      (document.getElementById('passwordInput') as HTMLInputElement).type =
        'password';
  }
}