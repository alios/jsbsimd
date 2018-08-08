<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" encoding="UTF-8"/>
  <xsl:template match="/runscript">
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta http-equiv="x-ua-compatible" content="ie=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <title><xsl:value-of select="@name"/></title>
        <meta name="description" content="a JSBSim Script" />
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
        <link rel="manifest" href="site.webmanifest" />
        <link rel="apple-touch-icon" href="icon.png" />
        <link href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous" />
      </head>
      <body>
        <h1>JSBSim Script: <xsl:value-of select="@name"/></h1>

        <p class="lead">
          <xsl:value-of select="description"/>
        </p>

        <h3>Run Configuration</h3>
        <dl class="row">
          <tr>
            <dt class="col-sm-3">Aircraft: </dt>
            <dd class="col-sm-9"><xsl:value-of select="use/@aircraft"/></dd>
          </tr>
          <tr>
            <dt class="col-sm-3">Initial Conditions: </dt>
            <dd class="col-sm-9"><xsl:value-of select="use/@initialize"/></dd>
          </tr>
          <tr>
            <dt class="col-sm-3">Starts at (sec): </dt>
            <dd class="col-sm-9"><xsl:value-of select="run/@start"/></dd>
          </tr>
          <tr>
            <dt class="col-sm-3">Ends at (sec): </dt>
            <dd class="col-sm-9"><xsl:value-of select="run/@end"/></dd>
          </tr>
          <tr>
            <dt class="col-sm-3">Delta time (sec): </dt>
            <dd class="col-sm-9"><xsl:value-of select="run/@dt"/></dd>
          </tr>
        </dl>

	<xsl:if test="run/property">
          <h3>Local Properties</h3>
	  <ul>
            <xsl:for-each select="run/property">
              <li><xsl:value-of select="."/></li>
            </xsl:for-each>
          </ul>
	</xsl:if>


        <table width="100%">
          <font face="Arial" size="2">

            <xsl:for-each select="run/event">
              <tr bgcolor="EEEEEE"><td><hr width="100%"/><font face="Arial" size="2" color="#0033ff"><b>Event</b>: <xsl:value-of select="@name"/></font>
              <xsl:if test="description"><font face="Arial" size="2"><br/><b>Description</b>: <xsl:value-of select="description"/></font></xsl:if>
              </td></tr>
              <tr><td>
                <font face="Arial" size="2">
                  <xsl:if test="condition">
                    <b>Test Conditions</b>:
                    <ul>
                      <xsl:for-each select="condition">
                        <li><xsl:value-of select="."/></li>
                      </xsl:for-each>
                    </ul>
                  </xsl:if>
                  <xsl:if test="set"> <!-- false if no set actions -->
                    <b>Actions</b>:
                    <xsl:if test="set">
                      <ul>
                        <xsl:for-each select="set">
                          <li>
                            <xsl:if test="@type">
                              Change <xsl:value-of select="@name"/> by <xsl:value-of select="@value"/>
                            </xsl:if>
                            <xsl:if test="not(@type)">
                              Set <xsl:value-of select="@name"/> to <xsl:value-of select="@value"/>
                            </xsl:if>
                            <xsl:if test="@action">
                              <xsl:if test="@action = 'FG_STEP'">
                                via step
                              </xsl:if>
                              <xsl:if test="@action = 'FG_EXP'">
                                via exponential input
                              </xsl:if>
                              <xsl:if test="@action = 'FG_RAMP'">
                                via ramp input
                              </xsl:if>
                              <xsl:if test="@tc">
                                over <xsl:value-of select="@tc"/> seconds
                              </xsl:if>
                            </xsl:if>
                          </li>
                        </xsl:for-each>
                      </ul>
                    </xsl:if>

                  </xsl:if> <!-- Actions -->

                  <xsl:if test="notify">
                    When this event is triggered, a notification message will be shown
                    <xsl:if test="notify/property">
                      and the values of following property or properties will be displayed:
                      <ul>
                        <xsl:for-each select="notify/property">
                          <li><xsl:value-of select="."/></li>
                        </xsl:for-each>
                      </ul>
                    </xsl:if>
                  </xsl:if>
                </font>
              </td></tr>
              <tr><td></td></tr>
            </xsl:for-each>
          </font>
        </table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
