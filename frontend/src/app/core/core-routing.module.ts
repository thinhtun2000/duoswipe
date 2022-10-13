import { NgModule } from '@angular/core';
import { Route, RouterModule } from '@angular/router';
import { LoginComponent } from '../auth/pages/login/login.component';
import { SimpleAppLayoutComponent } from './layouts/simple-app-layout/simple-app-layout.component';
import { HomePageComponent } from './pages/home-page/home-page.component';

const routes: Route[] = [
  {
    path: '',
    component: SimpleAppLayoutComponent,
    children: [{ path: 'home', component: HomePageComponent }],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class CoreRoutingModule {}
