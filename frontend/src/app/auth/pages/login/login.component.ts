import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { LoginRequest } from 'src/app/core/models/loginRequest';
import { AuthService } from 'src/app/core/services/auth/auth.service';
import { UserApiService } from 'src/app/core/services/user-api/user-api.service';
import { UserService } from 'src/app/core/services/user/user.service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss'],
})
export class LoginComponent implements OnInit {
  public form: FormGroup;
  public submitting: boolean = false;

  constructor(
    private fb: FormBuilder,
    private router: Router,
    private userApi: UserApiService,
    private userSvc: UserService,
    private authSvc: AuthService
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      username: ['', [Validators.required]],
      password: ['', [Validators.required]],
    });
  }

  public onSubmit(): void {
    const loginRequestObject = this.form.value as LoginRequest;
    this.authSvc.login(loginRequestObject).subscribe((response) => {
      console.log(response);
    });
    console.log('Hi');
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
