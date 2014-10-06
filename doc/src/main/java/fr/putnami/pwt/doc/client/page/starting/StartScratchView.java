/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.page.starting;

import com.google.common.collect.Lists;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import java.util.List;

import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.HTMLPanel;
import fr.putnami.pwt.core.widget.client.InputSwitch;

@Templated
public class StartScratchView extends Composite implements View {
	@UiField(provided = true)
	List<String> switchItems = Lists.newArrayList("GRADLE", "MAVEN");

	@UiField
	InputSwitch<String> switchBuild;

	@UiField
	HTMLPanel buildGradlePanel;
	@UiField
	HTMLPanel runGradlePanel;

	@UiField
	HTMLPanel buildMavenPanel;
	@UiField
	HTMLPanel runMavenPanel;

	@PostConstruct
	void postConstruct() {
		this.switchBuild.edit("GRADLE");
		this.buildMavenPanel.setVisible(false);
		this.runMavenPanel.setVisible(false);
	}

	@UiHandler("switchBuild")
	void onSwitch(ValueChangeEvent<String> event) {
		if ("MAVEN".equals(event.getValue())) {
			this.buildGradlePanel.setVisible(false);
			this.runGradlePanel.setVisible(false);
			this.buildMavenPanel.setVisible(true);
			this.runMavenPanel.setVisible(true);

		} else {
			this.buildGradlePanel.setVisible(true);
			this.runGradlePanel.setVisible(true);
			this.buildMavenPanel.setVisible(false);
			this.runMavenPanel.setVisible(false);
		}
	}
}
