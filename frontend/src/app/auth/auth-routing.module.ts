import { NgModule } from '@angular/core';
import { Route, RouterModule } from '@angular/router';
import { SimpleAppLayoutComponent } from '../core/layouts/simple-app-layout/simple-app-layout.component';
import { LoginComponent } from './pages/login/login.component';
import { RegisterComponent } from './pages/register/register.component';

const routes: Route[] = [
  {
    path: 'login',
    component: LoginComponent,
  },
  {
    path: 'signup',
    component: RegisterComponent,
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AuthRoutingModule {}
