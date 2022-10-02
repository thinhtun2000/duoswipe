import { NgModule } from '@angular/core';
import { Route, RouterModule } from '@angular/router';
import { AppLayoutComponent } from './layouts/app-layout/app-layout.component';
import { HomePageComponent } from './pages/home-page/home-page.component';

const routes: Route[] = [
  {
    path: '',
    component: AppLayoutComponent,
    children: [{ path: 'home', component: HomePageComponent }],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class CoreRoutingModule {}
