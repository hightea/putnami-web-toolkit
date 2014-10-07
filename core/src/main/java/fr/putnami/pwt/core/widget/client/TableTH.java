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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.TableCellElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.model.client.base.HasReadonly;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumn.ColumnVisibility;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumn.Type;

public class TableTH<T> extends AbstractPanel implements CloneableWidget, HasReadonly, HasDrawable {

	private Integer colspan;
	private Boolean readonly;
	private ColumnVisibility readonlyVisibility;
	private Type type = Type.DEFAULT;

	public TableTH() {
		super(TableCellElement.TAG_TH);
	}

	protected TableTH(TableTH<T> source) {
		super(source);
		this.colspan = source.colspan;

		this.cloneSourceWidgets(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableTH<T>(this);
	}

	public Integer getColspan() {
		return this.colspan;
	}

	public void setColspan(Integer colspan) {
		this.colspan = colspan;
	}

	@Override
	public void redraw() {
		if (this.colspan != null) {
			TableCellElement.as(this.getElement()).setColSpan(this.colspan);
		}
	}

	@Override
	public void add(IsWidget w) {
		this.append(w);
	}

	@Override
	public Boolean getReadonly() {
		return this.readonly;
	}

	@Override
	public void setReadonly(Boolean readonly) {
		this.readonly = readonly;
		this.renderVisible();
	}

	public ColumnVisibility getReadonlyVisibility() {
		return this.readonlyVisibility;
	}

	public void setReadonlyVisibility(ColumnVisibility readonlyVisibility) {
		this.readonlyVisibility = readonlyVisibility;
		this.renderVisible();
	}

	public void setType(Type type) {
		this.type = type;
	}

	public Type getType() {
		return this.type;
	}

	private void renderVisible() {
		if (this.readonlyVisibility == null) {
			this.setVisible(true);
		} else {
			switch (this.readonlyVisibility) {
				case VISIBLE:
					this.setVisible(true);
					break;
				case HIDE:
					this.setVisible(false);
					break;
				case HIDE_READONLY:
					this.setVisible(!Boolean.TRUE.equals(this.readonly));
					break;
				case VISIBLE_READONLY:
					this.setVisible(Boolean.TRUE.equals(this.readonly));
					break;
			}
		}
	}

}
