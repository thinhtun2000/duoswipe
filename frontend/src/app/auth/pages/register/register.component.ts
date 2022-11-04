import { HttpErrorResponse } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { catchError, of } from 'rxjs';
import { LoginRequest } from 'src/app/core/models/loginRequest';
import { LoginResponse } from 'src/app/core/models/loginResponse';
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
      email: ['', [Validators.required]],
      password: ['', [Validators.required]],
    });
  }

  public onSubmit(): void {
    const loginRequestObject = this.form.value as LoginRequest;
    this.authSvc
      .login(loginRequestObject)
      .subscribe((response: LoginResponse) => {
        switch (response.status) {
          case 'success':
            this.userApi
              .getUserById(response.user_id)
              .pipe(catchError((error: HttpErrorResponse) => of(error.error)))
              .subscribe({
                next: (response: User) => {
                  console.log(response);
                  if (response) {
                    console.log('fetch user success');
                    this.userSvc.setUser(response);
                    this.router.navigateByUrl('/app');
                  } else console.log('fetch user fail');
                },
              });
            break;
          default:
            console.log('fail');
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