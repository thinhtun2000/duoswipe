import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { RegisterRequest } from 'src/app/core/models/registerRequest';
import { RegisterResponse } from 'src/app/core/models/registerResponse';
import { AuthService } from 'src/app/core/services/auth/auth.service';

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
    private authSvc: AuthService
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      username: ['', [Validators.required]],
      email: ['', [Validators.required]],
      password: ['', [Validators.required]],
      confirm_password: ['', [Validators.required]],
    });
  }

  public onSubmit(): void {
    if (this.form.value.confirm_password == this.form.value.password) {
      const registerRequestObject = {
        username: this.form.value.username,
        email: this.form.value.email,
        password: this.form.value.password,
      } as RegisterRequest;
      console.log(registerRequestObject);
      this.authSvc
        .register(registerRequestObject)
        .subscribe((response: RegisterResponse) => {
          console.log(response.status);
          if (response.status === 'success') {
            this.router.navigateByUrl('/auth/login');
          } else console.log('register fail');
        });
    }
  }

  public changeVisibility(): void {
    var x = (document.getElementById('passwordInput') as HTMLInputElement).type;
    var y = (
      document.getElementById('confirmPasswordInput') as HTMLInputElement
    ).type;
    if (x === 'password')
      (document.getElementById('passwordInput') as HTMLInputElement).type =
        'text';
    else if (x === 'text')
      (document.getElementById('passwordInput') as HTMLInputElement).type =
        'password';
    if (y === 'password')
      (
        document.getElementById('confirmPasswordInput') as HTMLInputElement
      ).type = 'text';
    else if (y === 'text')
      (
        document.getElementById('confirmPasswordInput') as HTMLInputElement
      ).type = 'password';
  }
}
