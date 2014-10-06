/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import com.google.common.collect.Maps;
import com.google.gwt.dom.client.TableRowElement;
import com.google.gwt.dom.client.TableSectionElement;
import com.google.gwt.user.client.ui.IsWidget;

import java.util.Map;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumn;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumnAspect;
import fr.putnami.pwt.core.widget.client.base.HasHeaderCell;

public class TableHead extends AbstractPanel implements CloneableWidget {

	private final Container headerRow = new Container(TableRowElement.TAG);
	private Map<String, Container> aspectRows = Maps.newHashMap();

	public TableHead() {
		super(TableSectionElement.TAG_THEAD);
		append(headerRow);
	}

	protected TableHead(TableHead source) {
		super(source);
		append(headerRow);
		headerRow.cloneSourceWidgets(source.headerRow);
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableHead(this);
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof AbstractTableColumn) {
			this.addColumn((AbstractTableColumn) w);
		}
		if (w instanceof TableTH
				|| w instanceof TableTD) {
			headerRow.append(w);
		}
	}

	public void removeColumn(AbstractTableColumn column) {
		column.createHeaderCell().removeFromParent();
	}

	private <A> void addColumn(AbstractTableColumn<A> column) {
		TableTH<A> cell = column.createHeaderCell();

		headerRow.append(cell);

		for (AbstractTableColumnAspect<?> aspect : column.getAspects()) {
			if (aspect instanceof HasHeaderCell) {
				Container row = ensureAspectRow(aspect.getClass().getName());
				TableTH<?> aspectCell = new TableTH();
				aspectCell.setColspan(column.getColspan());
				row.append(aspectCell);
				((HasHeaderCell) aspect).setHeaderCell(aspectCell);
			}
		}

		for (Container existingRow : aspectRows.values()) {
			if (headerRow.getWidgetCount() > existingRow.getWidgetCount()) {
				TableTH<?> newCell = new TableTH();
				newCell.setColspan(column.getColspan());
				existingRow.append(newCell);
			}
		}
	}

	private Container ensureAspectRow(String aspectClass) {
		Container row = aspectRows.get(aspectClass);
		if (row == null) {
			row = new Container(TableRowElement.TAG);
			aspectRows.put(aspectClass, row);
			row.setVisible(false);
			for (int i = 0; i < headerRow.getWidgetCount() - 1; i++) {
				row.append(new TableTH());
			}
			append(row);
		}
		return row;
	}
}
