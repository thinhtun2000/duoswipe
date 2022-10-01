import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { UserProfileComponent } from './pages/user-profile/user-profile.component';
import { SharedModule } from '../shared/shared.module';
import { UserRoutingModule } from './user-routing.module';
import { RouterModule } from '@angular/router';

@NgModule({
  declarations: [UserProfileComponent],
  imports: [CommonModule, RouterModule, SharedModule, UserRoutingModule],
})
export class UserModule {}
