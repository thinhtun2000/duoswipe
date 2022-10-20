import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { UserInfoCardComponent } from './components/user-info-card/user-info-card.component';
import { CoreModule } from '../core/core.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { UserPreferencesCardComponent } from './components/user-preferences-card/user-preferences-card.component';

@NgModule({
  declarations: [UserInfoCardComponent, UserPreferencesCardComponent],
  imports: [CommonModule, CoreModule, FormsModule, ReactiveFormsModule],
  exports: [UserInfoCardComponent, UserPreferencesCardComponent],
})
export class SharedModule {}
