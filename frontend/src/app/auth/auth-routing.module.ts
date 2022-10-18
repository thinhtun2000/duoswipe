import { NgModule } from '@angular/core';
import { Route, RouterModule } from '@angular/router';
import { SimpleAppLayoutComponent } from '../core/layouts/simple-app-layout/simple-app-layout.component';
import { LoginComponent } from './pages/login/login.component';

const routes: Route[] = [
  {
    path: '',
    component: SimpleAppLayoutComponent,
    children: [
      {
        path: 'login',
        component: LoginComponent,
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AuthRoutingModule {}
